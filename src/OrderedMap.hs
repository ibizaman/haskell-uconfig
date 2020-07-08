{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module OrderedMap
    ( OrderedMap
    , fromList
    , toList
    , insert
    , insertWith
    , alter
    , empty
    , lookup
    , mapWithKeys
    , followOrderFrom
    , keys
    )
where

import           Prelude                 hiding ( lookup )
import qualified Data.List                     as L


newtype OrderedMap k v = OrderedMap [(k, v)]
    deriving (Show, Eq, Semigroup, Monoid)

fromList :: [(k, v)] -> OrderedMap k v
fromList = OrderedMap

toList :: OrderedMap k v -> [(k, v)]
toList (OrderedMap m) = m

insert :: (Eq k) => k -> v -> OrderedMap k v -> OrderedMap k v
insert k v (OrderedMap m) = OrderedMap $ insert' m
  where
    insert' [] = [(k, v)]
    insert' ((k', v') : r) =
        if k == k' then (k, v) : r else (k', v') : insert' r

insertWith
    :: (Eq k) => (v -> v -> v) -> k -> v -> OrderedMap k v -> OrderedMap k v
insertWith f k v (OrderedMap m) = OrderedMap $ insert' m
  where
    insert' [] = [(k, v)]
    insert' ((k', v') : r) =
        if k == k' then (k, f v v') : r else (k', v') : insert' r

alter :: (Eq k) => (Maybe v -> Maybe v) -> k -> OrderedMap k v -> OrderedMap k v
alter f k (OrderedMap m) = OrderedMap $ alter' m
  where
    alter' [] = f' k Nothing
    alter' ((k', v') : r) =
        if k == k' then f' k (Just v') else (k, v') : alter' r

    f' k'' v'' = case f v'' of
        Nothing   -> []
        Just v''' -> [(k'', v''')]

empty :: OrderedMap k v
empty = OrderedMap []

lookup :: (Eq k) => k -> OrderedMap k v -> Maybe v
lookup k (OrderedMap m) = L.lookup k m

mapWithKeys :: Monoid a => (k -> v -> a) -> OrderedMap k v -> a
mapWithKeys f (OrderedMap a) = mconcat $ fmap (uncurry f) a

followOrderFrom :: (Eq k) => OrderedMap k v -> OrderedMap k v -> OrderedMap k v
followOrderFrom m order =
    fromList
        $  mapWithKeys
               (\k _ -> case lookup k m of
                   Nothing -> []
                   Just v  -> [(k, v)]
               )
               order
        <> mapWithKeys
               (\k v -> case lookup k order of
                   Nothing -> [(k, v)]
                   Just _  -> []
               )
               m

keys :: OrderedMap k v -> [k]
keys (OrderedMap m) = fmap fst m
