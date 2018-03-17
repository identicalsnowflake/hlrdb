-- | HLRDB is an opinionated, high-level, type-driven library for modeling Redis-backed database architecture.
-- This package makes many decisions for you about how to serialize and deserialize values, construct identifiers, and define path names. If you want more control over these aspects, you may instead use the HLRDB Core package, which defines only the abstract API and does not opine on these matters.

module HLRDB
       (
         module HLRDB.Core
       , setExpireIn
       , setExpireAt
       , Identifier
       , IsIdentifier(..)
       , Store
       , genId
       , genId'
       , identifierTimestamp
       , declareBasic
       , declareIntegral
       , declareBasicZero
       , declareList
       , declareSet
       , declareHSet
       , declareSSet
       , declareGlobalBasic
       , declareGlobalIntegral
       , declareGlobalBasicZero
       , declareGlobalList
       , declareGlobalSet
       , declareGlobalHSet
       , declareGlobalSSet
       , encodePath
       , foldPath
       ) where

import HLRDB.Core
import HLRDB.Util

import Data.Functor.Identity
import Data.Time.Exts.Unix
import Data.Time.Exts.Base (Calendar(Gregorian))
import Database.Redis
import GHC.Int
import GHC.Generics
import Data.String (IsString(fromString))
import Data.Store
import Data.ByteString (ByteString,take,drop,unpack)
import qualified Data.ByteString
import qualified Crypto.Hash as H
import qualified Data.ByteArray as H
import Data.Monoid ((<>))
import System.Random
import GHC.Word
import Data.Bits
import Control.Monad
import qualified Data.ByteString.Base64 as B64
import Data.Hashable (Hashable)


-- | Declare identifiers using newtypes for Identifier, e.g.,
-- @newtype CommentId = CommentId Identifier deriving (Generic,Eq,Ord,IsIdentifier,Store,Hashable)@
newtype Identifier = Identifier (Int32,Word32,Word16,Word8) deriving (Generic,Eq,Ord,Hashable)

instance Show Identifier where
  show = show . B64.encode . encode

-- | IsIdentifier means that @a@ is isomorphic to Identifier, usually via newtype. This enables to use @genId :: (IsIdentifier a) => IO a@, declared below. It is required that not only is it isomorphic; it must respect the Store instance as well (you get this for free with a newtype anyway).
class IsIdentifier a where
  toIdentifier :: a -> Identifier
  fromIdentifier :: Identifier -> a

instance IsIdentifier Identifier where
  toIdentifier = id
  fromIdentifier = id

instance Store Identifier where
  size = ConstSize 11
  peek = Identifier <$> ((,,,) <$> peek <*> peek <*> peek <*> peek)
  poke (Identifier (a,b,c,d)) = poke a >> poke b >> poke c >> poke d


-- | Generate a new identifier using the current time as the timestamp
{-# INLINE genId #-}
genId :: (IsIdentifier a) => IO a
genId = getCurrentUnixDateTime >>= genId'

-- use an offset to make 32-bit timestamps last another 100 years
{-# INLINE offset #-}
offset :: Int64
offset = 2524608000 -- January 1, 2050

-- | Generate a new identifier for the given timestamp
genId' :: (IsIdentifier a) => UnixDateTime 'Gregorian -> IO a
genId' (UnixDateTime i64) = do
  let t :: Int32 = fromIntegral (i64 - offset)
  w64 :: Word64 <- randomIO
  let (a,w32) = w64tow32w32 w64
  let (b,x) = w32tow16w16 w32
  let (c,_) = w16tow8w8 x
  return $ fromIdentifier $ Identifier (t , a , b , c)
  where
    w64tow32w32 :: Word64 -> (Word32, Word32)
    w64tow32w32 i = (fromIntegral i , fromIntegral (rotate i 32))
    
    w32tow16w16 :: Word32 -> (Word16, Word16)
    w32tow16w16 i = (fromIntegral i , fromIntegral (rotate i 16))
    
    w16tow8w8 :: Word16 -> (Word8,Word8)
    w16tow8w8 i = (fromIntegral i , fromIntegral (rotate i 8))

-- | Extract the timestamp from an identifier
{-# INLINABLE identifierTimestamp #-}
identifierTimestamp :: (IsIdentifier a) => a -> (UnixDateTime 'Gregorian)
identifierTimestamp i =
  let (Identifier (t,_,_,_)) = toIdentifier i in
  UnixDateTime $ offset + fromIntegral t

-- Primitive redis key encoding scheme (16 bytes total):
-- 
-- 1. 5 bytes - 40-bit prefix of MD5 pathname hash;
--              note that this is a birthday problem - prob collision = birthday (2^5) (# of path names you use)
-- 2. 11-byte Identifier (including 32-bit timestamp)
-- This paradigm allows the following:
-- 1. iterating all indexes in a particular path
-- 2. efficiently discriminating which data is fresh
newtype PathName = PathName ByteString

instance IsString PathName where
  fromString =
      PathName
    . Data.ByteString.take 5
    . H.convert . H.hashFinalize
    . (H.hashUpdate (H.hashInit :: H.Context H.MD5) :: ByteString -> H.Context H.MD5)
    . fromString

encodePath :: (IsIdentifier a) => PathName -> a -> ByteString
encodePath (PathName n) =
  (<>) n . encode . toIdentifier


failDecode :: PeekException -> a
failDecode e = error $ "Unexpected data encoding from Redis: " <> show e

-- there should never be an incorrect encoding stored in Redis
{-# INLINE decode' #-}
decode' :: (Store a) => ByteString -> a
decode' bs = case Data.Store.decode bs of
  Left e -> failDecode e
  Right a -> a

-- structure declaration API

-- | Standard key-value store in Redis
{-# INLINE declareBasic #-}
declareBasic :: (IsIdentifier i, Store v) => PathName -> RedisBasic i (Maybe v)
declareBasic pathName = RKeyValue $
  E (encodePath pathName)
    (fmap encode)
    $ \case
      Just bs -> case Data.Store.decode bs of
        Left _ -> Nothing
        Right x -> Just x
      Nothing -> Nothing

-- | Standard key-value store, but backed by a primitive integer in Redis, enabling extra commands like @incr@
{-# INLINE declareIntegral #-}
declareIntegral :: (IsIdentifier i, Integral b) => PathName -> RedisIntegral i b
declareIntegral p = RKeyValueInteger (encodePath p) toInteger fromIntegral

-- | Allows defining your own "zero" value. An example might be RoseTree, where a non-existant value in Redis can be mapped to a sensible empty value in Haskell.
{-# INLINE declareBasicZero #-}
declareBasicZero :: (IsIdentifier i, Store v) => PathName -> v -> RedisBasic i v
declareBasicZero pathName zero = RKeyValue $
  E (encodePath pathName)
    (Just . encode)
    $ \case
       Nothing -> zero
       Just bs -> case Data.Store.decode bs of
         Left _ -> zero
         Right x -> x

-- | Standard Redis list, supporting prepends, appends, and range access. If a @TrimScheme@ is provided, operations will automatically trim the list to the specified length.
{-# INLINE declareList #-}
declareList :: (IsIdentifier i, Store v) => PathName -> Maybe TrimScheme -> RedisList i v
declareList pathName = RList $ E (encodePath pathName) (pure . encode) (decode' . runIdentity)

-- | A sub-hash table, using the sub-index type @s@. @s@ here is only required to be Storable rather than IsIdentifier, but in practice you'll probably use identifiers for @s@, too.
{-# INLINE declareHSet #-}
declareHSet :: (IsIdentifier i, Store s, Store v) => PathName -> RedisHSet i s v
declareHSet pathName =
  RHSet (E (encodePath pathName) (pure . encode) (decode' . runIdentity)) (HSET encode decode')

-- | A set in Redis. Note that your Haskell Eq should respect the equality via Serialize, since Redis set operations will be operating on the binary equality, not your Haskell Eq instance.
{-# INLINE declareSet #-}
declareSet :: (IsIdentifier i, Store v) => PathName -> RedisSet i v
declareSet pathName = RSet $ E (encodePath pathName) (pure . encode) (decode' . runIdentity)

-- | A sorted set in Redis. You may optionally provide a trim scheme, which will automatically manage keeping the set at a maximum size for you, 
{-# INLINE declareSSet #-}
declareSSet :: (IsIdentifier i, Store v) => PathName -> Maybe TrimScheme -> RedisSSet i v
declareSSet pathName = RSortedSet $ E (encodePath pathName) (pure . encode) (decode' . runIdentity)

-- | Unindexed (global) paths
-- You may also declare global paths, which are indexed simply by (), rather than an Identifier newtype.

{-# INLINE declareGlobalBasic #-}
declareGlobalBasic :: (Store v) => PathName -> RedisBasic () (Maybe v)
declareGlobalBasic (PathName p) = RKeyValue $ E (const p) (fmap encode) $ \case
  Just bs -> case Data.Store.decode bs of
    Left _ -> Nothing
    Right x -> Just x
  Nothing -> Nothing

-- | A global version of @declareIntegral@
{-# INLINE declareGlobalIntegral #-}
declareGlobalIntegral :: (Integral b) => PathName -> RedisIntegral () b
declareGlobalIntegral (PathName p) = RKeyValueInteger (const p) toInteger fromIntegral

-- | A global version of @declareZero@
{-# INLINE declareGlobalBasicZero #-}
declareGlobalBasicZero :: (Store v) => PathName -> v -> RedisBasic () v
declareGlobalBasicZero (PathName p) zero = RKeyValue $
  E (const p)
    (Just . encode)
    $ \case
       Nothing -> zero
       Just bs -> case Data.Store.decode bs of
         Left _ -> zero
         Right x -> x

-- | A global version of @declareList@
{-# INLINE declareGlobalList #-}
declareGlobalList :: (Store v) => PathName -> Maybe TrimScheme -> RedisList () v
declareGlobalList (PathName p) = RList $ E (const p) (pure . encode) (decode' . runIdentity)

-- | A global version of @declareHSet@
{-# INLINE declareGlobalHSet #-}
declareGlobalHSet :: (Store s, Store v) => PathName -> RedisHSet () s v
declareGlobalHSet (PathName p) =
  RHSet (E (const p) (pure . encode) (decode' . runIdentity)) (HSET encode decode')

-- | A global version of @declareSet@
{-# INLINE declareGlobalSet #-}
declareGlobalSet :: (Store v) => PathName -> RedisSet () v
declareGlobalSet (PathName p) = RSet $ E (const p) (pure . encode) (decode' . runIdentity)

-- | A global version of @declareSSet@
{-# INLINE declareGlobalSSet #-}
declareGlobalSSet :: (Store v) => PathName -> Maybe TrimScheme -> RedisSSet () v
declareGlobalSSet (PathName p) = RSortedSet $ E (const p) (pure . encode) (decode' . runIdentity)


-- data expiration

type Seconds = Integer

setExpireIn :: RedisStructure v a b -> a -> Seconds -> Redis ()
setExpireIn p k = ignore . expire (primKey p k)

setExpireAt :: RedisStructure v a b -> a -> UnixDateTime 'Gregorian -> Redis ()
setExpireAt p k (UnixDateTime t) = ignore $ expireat (primKey p k) (toInteger t)

-- | Generic iteration
-- Note that despite the pretty type signature, the actual implementation in Redis is slow (it uses the global scan command, so its run time is proportional to the number of total keys in Redis, *not* the number of keys specifically related to the given path). You should only use @foldPath@ for administrative tasks, and never for any public API.

scanGlob :: (IsIdentifier i) => RedisStructure s i v -> ByteString
scanGlob = pathGlob . extractPathName
  where
    pathGlob :: ByteString -> ByteString
    pathGlob p =
      let bs :: [ Word8 ] = unpack p in
      foldr (\c a -> enc c <> a) "*" bs
      where
        -- Redis matches via glob-style patterns, so need to be
        -- careful to escape the special characters
        enc :: Word8 -> ByteString
        enc 42 = "\\*"
        enc 63 = "\\?"
        enc 91 = "\\["
        enc w = Data.ByteString.pack [ w ]
    
    extractPathName :: (IsIdentifier i) => RedisStructure s i v -> ByteString
    extractPathName p = Data.ByteString.take 5 $ primKey p zeroIdentifier
      where
        zeroIdentifier :: (IsIdentifier i) => i
        zeroIdentifier = fromIdentifier $ Identifier (0,0,0,0)

foldPath :: (IsIdentifier i, Store v) => RedisStructure s i v -> (a -> i -> Redis a) -> a -> Redis a
foldPath p f z = go (cursor0,z)
  where
    go (c,a) = do
      (c', bs) <- unwrap $ scanOpts c defaultScanOpts { scanMatch = Just m }
      !a' <- Control.Monad.foldM (\x -> f x . fromIdentifier . decodeEx . Data.ByteString.drop 5) a bs
      if c' == cursor0
         then pure a'
         else go (c',a')
    
    m = scanGlob p

