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
    )
where

import           Control.Applicative            ( (<|>) )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.Text                     as T
import qualified Parser                        as P



data Op a
    = Add a
    | Remove a
    | Replace a
    | Erase
    | Disable
    | Enable
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
            P.try (P.chunk "=" >> P.eol >> return Erase)
            <|> (Replace <$> (P.chunk "=" >> P.line))
            <|> (Add <$> (P.chunk "+=" >> P.line))
            <|> (Remove <$> (P.chunk "-=" >> P.line))
            <|> (P.chunk "#-" >> return Enable)
            <|> (P.chunk "#+" >> return Disable)

        return $ SyntaxModifier [(newKey key, op)]

    mergeEither = foldl f mempty
      where
        f (es, vs) (Left  e) = (es <> [e], vs)
        f (es, vs) (Right v) = (es, vs <> v)
