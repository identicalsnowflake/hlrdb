# High-level Redis Database

HLRDB is an opinionated, high-level, type-driven library for modeling Redis-backed database architecture.

This package makes many decisions for you about how to serialize and deserialize values, construct identifiers, and define path names. If you want more control over these aspects, you may instead use the HLRDB Core package, which defines only the abstract API and does not opine on these matters.


## Overview

Redis is a hash table database with several builtin primitive data structures. It does not use SQL, but instead uses [its own system of primitive commands](https://redis.io/commands). You may find primitive Haskell bindings for these commands [in the Hedis library](https://hackage.haskell.org/package/hedis), on which this library depends. HLRDB provides a type-driven, high-level abstraction on top of this.

## Identifiers

Use newtypes for `Identifier` for your various data types:

```haskell

newtype CommentId = CommentId Identifier deriving (Eq,Ord,Show,Store,IsIdentifier)

-- use genId to create new identifiers:
example :: IO CommentId
example = genId
```

## Data structures

Redis structures are mostly indexed by two types: their identifier and their value.

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

```haskell
-- hset, basically a sub-hash table with a few extra primitive commands
voteHSet :: RedisStructure (HSET CommentId) UserId Vote
voteHSet = declareHSet "whether a user has voted a comment up or down"

-- list, with automatic max-length management
tidToComments :: RedisList ThreadId CommentId
tidToComments = declareList "non-recursive comment threads" $ Just 1000

-- sorted sets are fairly complicated; they store a set of items ordered by score.
-- the second parameter expresses the trim scheme (which is more complicated than List due to Redis's API)
popularItems :: RedisSSet UserId PostId
popularItems = declareSSet "popular content" $ Just (1000, Just 0.01) -- 1k max; trim with probability 0.01

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

Once you've declared any of the above structures, you may use the Redis monad to perform operations on them. You may find the operations available for each structure defined in the `HLRDB/Structures` folder for that particular structure. The commands are similar to the original Redis API, but have been cleaned up a bit to support more of a Haskell dialect (e.g., list commands do not crash when passed `[]` as they do in Redis).

## Lookup Aggregation

You may construct a single query using many `RedisBasic` paths, which will result in a single `mget` command being executed in Redis. This allows constructing detailed data views in an efficient manner. I've written an [in-depth article discussing this](https://identicalsnowflake.github.io/QueryAggregation.html), but the gist of it is the query is `Traversing` (from Profunctors), `Applicative`, and you may combine any two queries into a single query (i.e., it's a monoid, but not in the Haskell typeclass sense).

Note that aggregation is only available on key-value (`RedisBasic`) paths: Redis does not support looking up other data structures as part of an `mget` query.
