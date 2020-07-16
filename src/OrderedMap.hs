{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Module      : OrderedMap
Description : Like 'Data.Map' with user-defined key order
Copyright   : (c) Pierre Penninckx, 2020
License     : GPL-3
Maintainer  : Pierre Penninckx (ibizapeanut@gmail.com)
Stability   : experimental
Portability : POSIX

Provides an 'OrderedMap' data structure that resembles a 'Data.Map'
but remembers the insertion order. It does not allow duplicate keys.

Provides also functions to modify and query from an 'OrderedMap' as
well as to modify the order of the keys inside the 'OrderedMap'.
-}
module OrderedMap
    ( OrderedMap
    , fromList
    , toList
    , insert
    , insertWith
    , alter
    , empty
    , lookup
    , lookupDefault
    , foldWithKeys
    , followOrderFrom
    , keys
    )
where

import           Prelude                 hiding ( lookup )
import qualified Data.List                     as L
import qualified Data.Maybe                    as M


-- |A Map of keys 'k' and values 'v' that stores also the order of the
-- keys. Think of it as a list of key-value pairs.
newtype OrderedMap k v = OrderedMap [(k, v)]
    deriving (Show, Eq, Semigroup, Monoid)

-- |Builds an 'OrderedMap' from a list of tuples. Retains the order of
-- keys from the list.
--
-- >>> fromList [("a", 1), ("b", 2)]
-- OrderedMap [("a",1),("b",2)]
fromList :: [(k, v)] -> OrderedMap k v
fromList = OrderedMap

-- |Returns a list of tuples, one tuple for each key-value pair, in
-- the order of the 'OrderedMap'.
--
-- >>> toList $ fromList [("a", 1), ("b", 2)]
-- [("a",1),("b",2)]
toList :: OrderedMap k v -> [(k, v)]
toList (OrderedMap m) = m

-- |Adds a key-value pair to an 'OrderedMap'. If the key already
-- exists, its value is replaced in its order retained. If not, the
-- key-value pair is added at the end.
--
-- >>> insert "a" 2 $ fromList [("a", 1), ("b", 2)]
-- OrderedMap [("a",2),("b",2)]
--
-- >>> insert "c" 3 $ fromList [("a", 1), ("b", 2)]
-- OrderedMap [("a",1),("b",2),("c",3)]
insert :: (Eq k) => k -> v -> OrderedMap k v -> OrderedMap k v
insert k v (OrderedMap m) = OrderedMap $ insert' m
  where
    insert' [] = [(k, v)]
    insert' ((k', v') : r) =
        if k == k' then (k, v) : r else (k', v') : insert' r

-- |Adds a key-value pair to an 'OrderedMap'. If the key already
-- exists, the old value and new value are merged using the
-- user-provided function while the key order is retained. If not, the
-- key-value pair is added at the end.
--
-- >>> insertWith (<>) "a" [3] $ fromList [("a", [1]), ("b", [2])]
-- OrderedMap [("a",[3,1]),("b",[2])]
--
-- >>> insertWith (<>) "c" [3] $ fromList [("a", [1]), ("b", [2])]
-- OrderedMap [("a",[1]),("b",[2]),("c",[3])]
insertWith
    :: (Eq k) => (v -> v -> v) -> k -> v -> OrderedMap k v -> OrderedMap k v
insertWith f k v (OrderedMap m) = OrderedMap $ insert' m
  where
    insert' [] = [(k, v)]
    insert' ((k', v') : r) =
        if k == k' then (k, f v v') : r else (k', v') : insert' r

-- |Modifies a key using a user provided function. The function
-- receives a 'Just v' if the key exists in the 'OrderedMap' and a
-- 'Nothing' if not. The function modifies or creates the value by
-- returning a 'Just v' and deletes the key-value pair by returning a
-- 'Nothing'.
--
-- >>> :{
--     let f = \v ->
--              case v of
--                Just v' ->
--                  if v' == 1
--                  then Nothing
--                  else Just $ v' + 1
--                Nothing ->
--                  Just 0
--     :}
--
-- >>> alter f "a" $ fromList [("a", 1), ("b", 2)]
-- OrderedMap [("b",2)]
--
-- >>> alter f "b" $ fromList [("a", 1), ("b", 2)]
-- OrderedMap [("a",1),("b",3)]
--
-- >>> alter f "c" $ fromList [("a", 1), ("b", 2)]
-- OrderedMap [("a",1),("b",2),("c",0)]
alter :: (Eq k) => (Maybe v -> Maybe v) -> k -> OrderedMap k v -> OrderedMap k v
alter f k (OrderedMap m) = OrderedMap $ alter' m
  where
    alter' [] = f' k Nothing
    alter' ((k', v') : r) =
        if k == k' then f' k (Just v') <> r else (k', v') : alter' r

    f' k'' v'' = case f v'' of
        Nothing   -> []
        Just v''' -> [(k'', v''')]

-- |An empty 'OrderedMap'.
empty :: OrderedMap k v
empty = OrderedMap []

-- |Gets a value from an 'OrderedMap'. Returns 'Just v' if the key
-- exists, 'Nothing' if not.
--
-- >>> lookup "a" $ fromList [("a", 1), ("b", 2)]
-- Just 1
--
-- >>> lookup "c" $ fromList [("a", 1), ("b", 2)]
-- Nothing
lookup
    :: (Eq k)
    => k -- ^Key to lookup in the 'OrderedMap'
    -> OrderedMap k v
    -> Maybe v
lookup k (OrderedMap m) = L.lookup k m

-- |Gets a value from an 'OrderedMap' if it exists, otherwise returns
-- the given default value.
--
-- >>> lookupDefault "a" 3 $ fromList [("a", 1), ("b", 2)]
-- 1
--
-- >>> lookupDefault "c" 3 $ fromList [("a", 1), ("b", 2)]
-- 3
lookupDefault
    :: (Eq k)
    => k -- ^Key to lookup in the 'OrderedMap'
    -> v -- ^Default value if the key does not exist.
    -> OrderedMap k v
    -> v
lookupDefault k v m = M.fromMaybe v (lookup k m)

-- |Folds over all key-value pairs using 'Monoid's.
--
-- >>> foldWithKeys (\_ v -> [v]) $ fromList [("a", 1), ("b", 2)]
-- [1,2]
--
-- >>> foldWithKeys (\k _ -> [k]) $ fromList [("a", 1), ("b", 2)]
-- ["a","b"]
foldWithKeys :: Monoid a => (k -> v -> a) -> OrderedMap k v -> a
foldWithKeys f (OrderedMap a) = mconcat $ fmap (uncurry f) a

-- |Re-order keys from first 'OrderedMap' to follow order from second
-- 'OrderedMap'. All keys that do not exist in the second map are
-- moved to the end, keeping their relative order.
--
-- >>> followOrderFrom (fromList [("d", 4), ("b", 2), ("c", 3), ("a", 1)]) (fromList [("a", 'o'), ("b", 'o'), ("z", 'x')])
-- OrderedMap [("a",1),("b",2),("d",4),("c",3)]
followOrderFrom :: (Eq k) => OrderedMap k v -> OrderedMap k v' -> OrderedMap k v
followOrderFrom m order =
    fromList
        $  foldWithKeys
               (\k _ -> case lookup k m of
                   Nothing -> []
                   Just v  -> [(k, v)]
               )
               order
        <> foldWithKeys
               (\k v -> case lookup k order of
                   Nothing -> [(k, v)]
                   Just _  -> []
               )
               m

-- |Returns all keys in the same order as they are in the 'OrderedMap'.
--
-- >>> keys $ fromList [("d", 4), ("b", 2), ("c", 3), ("a", 1)]
-- ["d", "b", "c", "a"]
keys :: OrderedMap k v -> [k]
keys (OrderedMap m) = fmap fst m
