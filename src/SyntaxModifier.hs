{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : SyntaxModifier
Description : Generic modification of syntax tree
Copyright   : (c) Pierre Penninckx, 2020
License     : GPL-3
Maintainer  : Pierre Penninckx (ibizapeanut@gmail.com)
Stability   : experimental
Portability : POSIX

The SyntaxModifier provides a typeclass and functions to modify any syntax tree.
-}
module SyntaxModifier
    ( SyntaxModifier
    , apply
    , Updatable(..)
    , Op(..)
    , Key
    , keyPop

    -- Create
    , ConstructResult
    , construct
    , constructErrors
    , constructResult

      -- FieldsTree related
    , FieldsTree(..)
    , Path(..)
    , fieldsTree
    , path
    , pathValue
    )
where

import           Control.Applicative            ( (<|>) )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T
import qualified Parser                        as P



data Op a
    = Add a
    | Remove a
    | Replace a
    | Erase
    deriving(Show, Eq, Functor)

data Key = Key T.Text [T.Text]
    deriving(Show, Eq)

newKey :: NonEmpty T.Text -> Key
newKey (k :| ks) = Key k ks

keyPop :: Key -> (Maybe Key, T.Text)
keyPop (Key t []      ) = (Nothing, t)
keyPop (Key t (r : rs)) = (Just (Key r rs), t)


class Updatable o e a where
    update :: Key -> Op o -> a -> (e, a)

newtype SyntaxModifier o = SyntaxModifier {
      unSyntaxModifier :: [(Key, Op o)]
    }
    deriving(Semigroup, Monoid)

instance (Show o) => Show (SyntaxModifier o) where
    show (SyntaxModifier sm) = show sm

apply :: (Monoid e, Updatable o e a) => SyntaxModifier o -> a -> (e, a)
apply sm u = foldl
    (\(e', u') (k, op) -> let (e'', u'') = update k op u' in (e' <> e'', u''))
    (mempty, u)
    (unSyntaxModifier sm)


data ConstructError
    = UnsupportedOperation T.Text T.Text
    | EmptyKey T.Text

instance Show ConstructError where
    show (UnsupportedOperation line op) =
        T.unpack
            $  "Unsupported operation '"
            <> op
            <> "', must be one of '=', '+=', '-=' in '"
            <> line
            <> "'"
    show (EmptyKey line) = T.unpack $ "Key cannot be empty in '" <> line <> "'"

newtype ConstructResult = ConstructResult ([T.Text], SyntaxModifier T.Text)
    deriving(Semigroup, Monoid)

constructErrors :: ConstructResult -> [T.Text]
constructErrors (ConstructResult (errs, _)) = errs

constructResult :: ConstructResult -> SyntaxModifier T.Text
constructResult (ConstructResult (_, sm)) = sm

construct :: [T.Text] -> ConstructResult
construct ts = ConstructResult $ mergeEither $ fmap parse ts
  where
    parse = P.parse $ do
        key <- P.wordsSepBy (P.word' []) (P.chunk ".")
        op  <-
            (P.chunk "=" >> P.eol >> return Erase)
            <|> (Replace <$> (P.chunk "=" >> P.line))
            <|> (Add <$> (P.chunk "+=" >> P.line))
            <|> (Remove <$> (P.chunk "-=" >> P.line))

        return $ SyntaxModifier [(newKey key, op)]

    mergeEither = foldl f mempty
      where
        f (es, vs) (Left  e) = (es <> [e], vs)
        f (es, vs) (Right v) = (es, vs <> v)


data FieldsTree = FieldsTree (Maybe [T.Text]) (Map T.Text [FieldsTree])
  deriving (Eq, Show)

instance Semigroup FieldsTree where
    FieldsTree aValues aFields <> FieldsTree bValues bFields =
        FieldsTree (aValues <> bValues) (Map.unionWith (<>) aFields bFields)

instance Monoid FieldsTree where
    mempty = FieldsTree Nothing Map.empty


fieldsTree :: [(Path, T.Text)] -> FieldsTree
fieldsTree = foldr upsert mempty
  where
    upsert :: (Path, T.Text) -> FieldsTree -> FieldsTree
    upsert (Path [], newValue) (FieldsTree values fields) =
        FieldsTree (Just [newValue] <> values) fields
    upsert (Path (p : pathRest), newValue) (FieldsTree values fields) =
        case Map.lookup p fields of
            Just children -> FieldsTree values $ Map.insert
                p
                (map (upsert (Path pathRest, newValue)) children)
                fields
            Nothing -> FieldsTree
                values
                (Map.insert p [singleton (Path pathRest, newValue)] fields)

    singleton :: (Path, T.Text) -> FieldsTree
    singleton (Path [], value) = FieldsTree (Just [value]) Map.empty
    singleton (Path (p : pathRest), value) =
        FieldsTree Nothing (Map.singleton p [singleton (Path pathRest, value)])

pathValue :: P.Parser (Path, T.Text)
pathValue = do
    p <- path
    _ <- P.chunk "="
    v <- P.words
    return (p, v)


-- |Path is a location in a 'Config'.
newtype Path = Path [T.Text]
    deriving(Eq)

instance Show Path where
    show (Path p) = T.unpack $ T.intercalate "." p

path :: P.Parser Path
path = fmap (Path . T.splitOn ".") P.words
