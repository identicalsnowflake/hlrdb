# High-level Redis Database

HLRDB is an opinionated, high-level, type-driven library for modeling Redis-backed database architecture.

This package provides an easy API for you to declare your data paths in Redis, but in doing so makes many decisions for you about how to serialize and deserialize values, construct identifiers, and define path names. If you want more control over these aspects, you may instead use the HLRDB Core package, which simply defines the commands and the abstract API without opining on these matters.

## Overview

Redis is a hash table database with several builtin primitive data structures. It does not use SQL, but instead uses [its own system of primitive commands](https://redis.io/commands). You may find primitive Haskell bindings for these commands [in the Hedis library](https://hackage.haskell.org/package/hedis), on which this library depends. HLRDB provides a type-driven, high-level abstraction on top of this.

```haskell
-- minimal end-to-end, runnable example
import Data.Store
import Database.Redis (checkedConnect,defaultConnectInfo,runRedis)
import HLRDB

newtype CommentId = CommentId Identifier deriving (Eq,Ord,Show,Store,IsIdentifier)
newtype Comment = Comment String deriving (Eq,Ord,Show,Store)

cidToComment :: RedisBasic CommentId (Maybe Comment)
cidToComment = declareBasic "canonical mapping from CommentId to Comment"

main :: IO ()
main = do
  -- connect to Redis
  rconn <- checkedConnect defaultConnectInfo

  cid :: CommentId <- genId

  c :: Maybe Comment <- runRedis rconn $ do
    -- create a comment
    set' cidToComment cid $ Comment "hi"
    -- read it back
    get cidToComment cid

  print c

```

## Identifiers

Use newtypes for `Identifier` for your various data types:

```haskell

newtype CommentId = CommentId Identifier deriving (Eq,Ord,Show,Store,IsIdentifier)

-- use genId to create new identifiers:
example :: IO CommentId
example = genId
```

## Data structures

Redis structures are mostly indexed by two types: their identifier and their value. When you declare a structure, you need to provide a unique description, which serves two purposes: first, it helps document what the purpose of the path is, and second, the hash of this string is how HLRDB distinguishes between multiple paths of the same type.

### Basic

```haskell
-- RedisBasic is used when for standard key-value storage.
cidToComment :: RedisBasic CommentId (Maybe Comment)
cidToComment = declareBasic "canonical mapping from CommentId to Comment"

-- RedisIntegral will treat a non-existent value as 0
cidToScore :: RedisIntegral CommentId Integer
cidToScore = declareIntegral "comment score"

-- Use `declareBasicZero` to choose your own "zero" for the data type
threadIdToComments :: RedisBasic ThreadId (RoseTree CommentId)
threadIdToComments = declareBasicZero "reddit-style comment threads" Empty
```

### Other Redis structures

For lists and sorted sets, you may optionally provide a `TrimScheme` (a record with two fields, `softCardinality :: Integer` and `trimProbability :: Double`). When provided, HLRDB will automatically trim the structures in Redis to their proper size whenever data is added.

```haskell
-- hset, basically a sub-hash table with a few extra primitive commands
voteHSet :: RedisStructure (HSET CommentId) UserId Vote
voteHSet = declareHSet "whether a user has voted a comment up or down"

-- list, with automatic max-length management with TrimScheme
tidToComments :: RedisList ThreadId CommentId
tidToComments = declareList "non-recursive comment threads" $ Just $ TrimScheme 1000 0.1

-- sorted sets store items by golf score - lower is better. supports TrimScheme
popularItems :: RedisSSet UserId PostId
popularItems = declareSSet "popular content" $ Just $ TrimScheme 1000 0.01 -- 1k max; trim with probability 0.01

-- set is intuitive
blockedUsers :: RedisSet UserId UserId
blockedUsers = declareSet "a user's block list"

```

### Global paths

You may use the global variants of the above to declare paths indexed simply on `()`, rather than an `Identifier` newtype:

```haskell
bannedUsers :: RedisSet () UserId
bannedUsers = declareGlobalSet "global ban list"
```

Once you've declared any of the above structures, you may use the Redis monad to perform operations on them. You may find the operations available for each structure defined in the [HLRDB/Structures](https://github.com/identicalsnowflake/hlrdb-core/tree/master/src/HLRDB/Structures) folder (found in hlrdb-core) for that particular structure. The commands are similar to the original Redis API, but have been cleaned up and re-imagined to support more of a Haskell dialect (e.g., list commands do not crash when passed `[]` as they do in Redis).

## Lookup Aggregation

You may lift `RedisBasic i v` (and `RedisIntegral i v`, which is a subtype) paths to `i ⟿ v` queries, which can be combined together in several ways, resulting in a single `mget` command being executed in Redis. This allows constructing detailed data views in an efficient manner.

If you prefer, `Query i v` is a non-infix alias for `i ⟿ v`.

```haskell

newtype Views = Views Integer deriving (Show,Eq,Ord,Num,Enum,Real,Integral)
newtype Likes = Likes Integer deriving (Show,Eq,Ord,Num,Enum,Real,Integral)


cidToViews :: RedisIntegral CommentId Views
cidToViews = declareIntegral "comment views"

cidToLikes :: RedisIntegral CommentId Likes
cidToLikes = declareIntegral "comment likes"


queryBoth :: CommentId ⟿ (Views , Likes)
queryBoth = (,) <$> liftq cidToViews <*> liftq cidToLikes

reifyToRedis :: CommentId -> Redis (Views , Likes)
reifyToRedis = mget queryBoth

```

I've written an [in-depth article discussing aggregation here](https://identicalsnowflake.github.io/QueryAggregation.html), but the two most important takeaways are that `⟿` is a `Traversing` `Profunctor` and an `Applicative`.

## Demo

There is a [simple demo repository](https://github.com/identicalsnowflake/hlrdb-demo) demonstrating the end-to-end process of defining data models and performing read/write actions.
