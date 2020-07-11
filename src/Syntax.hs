{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : Syntax
Description : Helpers to create Parsers and Generators
Copyright   : (c) Pierre Penninckx, 2020
License     : GPL-3
Maintainer  : Pierre Penninckx (ibizapeanut@gmail.com)
Stability   : experimental
Portability : POSIX

Helpers to create Parsers and Generators.
-}
module Syntax
    ( XDGDesktop(..)
    , Sections
    , newSections
    , unSections
    , Section
    , newSection
    , unSection
    , Value(value, enabled, preComments, postComments)
    , newValue
    , setEnabled
    , Comment(unComment)
    , newComment
    , (/*)
    , (/*.)
    , (/**)
    , (/**.)
    , (<#)
    , (#>)

    -- Query
    , getMainSection
    , getSection
    , getValue
    )
where


import qualified Data.List                     as L
import qualified Data.Maybe                    as M
import qualified Data.String
import qualified Data.Text                     as T
import qualified SyntaxModifier                as SM
import qualified OrderedMap                    as OM


data XDGDesktop = XDGDesktop
    { firstSection :: Section
    , firstComments :: Comment
    , sections :: Sections
    , trailingComments :: Comment
    }
    deriving (Show, Eq)

instance Semigroup XDGDesktop where
    a <> b = XDGDesktop
        { firstSection     = firstSection a <> firstSection b
        , firstComments    = firstComments a <> firstComments b
        , sections         = sections a <> sections b
        , trailingComments = trailingComments a <> trailingComments b
        }

instance Monoid XDGDesktop where
    mempty = XDGDesktop { firstSection     = mempty
                        , firstComments    = mempty
                        , sections         = mempty
                        , trailingComments = mempty
                        }

(/*) :: XDGDesktop -> (Maybe T.Text, Section) -> XDGDesktop
x /* (Just k, s) = x
    { sections = let (Sections s') = sections x in Sections $ OM.insert k s s'
    }
x /* (Nothing, s) = x { firstSection = s }

(/*.)
    :: XDGDesktop
    -> (Maybe T.Text, Maybe Section -> Maybe Section)
    -> XDGDesktop
x /*. (Just k, f) = x
    { sections = let (Sections s') = sections x in Sections $ OM.alter f k s'
    }
x /*. (Nothing, f) = x
    { firstSection = case f (Just $ firstSection x) of
                         Nothing -> mempty
                         Just s' -> s'
    }


newtype Sections = Sections {
      unSections :: OM.OrderedMap T.Text Section
    }
    deriving (Show, Eq)

newSections :: [(T.Text, Section)] -> Sections
newSections = Sections . OM.fromList

instance Semigroup Sections where
    Sections a <> Sections b = Sections $ a <> b

instance Monoid Sections where
    mempty = Sections OM.empty


newtype Section = Section {
      unSection :: OM.OrderedMap T.Text [Value T.Text]
    }
    deriving (Show, Eq)

newSection :: OM.OrderedMap T.Text [Value T.Text] -> Section
newSection = Section

instance Semigroup Section where
    Section a <> Section b = Section $ a <> b

instance Monoid Section where
    mempty = Section OM.empty


(/**) :: Section -> (T.Text, [Value T.Text]) -> Section
(Section s) /** (k, v) = Section $ OM.insertWith (flip (<>)) k v s

(/**.)
    :: Section
    -> (T.Text, Maybe [Value T.Text] -> Maybe [Value T.Text])
    -> Section
(Section s) /**. (k, f) = Section $ OM.alter f k s


data Value v = Value
    { value :: v
    , enabled :: Bool
    , preComments :: Comment
    , postComments :: Comment
    }
    deriving (Show, Eq)

newValue :: v -> Value v
newValue v = Value { value        = v
                   , enabled      = True
                   , preComments  = Comment []
                   , postComments = Comment []
                   }

instance Data.String.IsString v => Data.String.IsString (Value v) where
    fromString = newValue . Data.String.fromString

instance Functor Value where
    fmap f v = v { value = f (value v) }

instance Applicative Value where
    pure = newValue
    Value { value = f } <*> v = v { value = f (value v) }

instance (Semigroup v) => Semigroup (Value v) where
    Value { value = v1, enabled = _, preComments = preC1, postComments = postC1 } <> Value { value = v2, enabled = enabled2, preComments = preC2, postComments = postC2 }
        = Value { value        = v1 <> v2
                , enabled      = enabled2
                , preComments  = preC1 <> preC2
                , postComments = postC1 <> postC2
                }

instance (Monoid v) => Monoid (Value v) where
    mempty = newValue mempty

(<#) :: Value v -> Comment -> Value v
v <# c = v { preComments = c }

(#>) :: Value v -> Comment -> Value v
v #> c = v { postComments = c }

setEnabled :: Bool -> Value v -> Value v
setEnabled b v = v { enabled = b }


newtype Comment = Comment {
      unComment ::  [T.Text]
    }
    deriving (Show, Eq)

newComment :: [T.Text] -> Comment
newComment = Comment

instance Semigroup Comment where
    Comment a <> Comment b = Comment $ a <> b

instance Monoid Comment where
    mempty = Comment []

instance Data.String.IsString Comment where
    fromString s = Comment $ T.pack <$> lines s


getMainSection :: XDGDesktop -> Section
getMainSection = firstSection

getSection :: XDGDesktop -> T.Text -> Section
getSection x name =
    M.fromMaybe mempty $ OM.lookup name $ unSections $ sections x

getValue :: Section -> T.Text -> [Value T.Text]
getValue s name = M.fromMaybe mempty $ OM.lookup name $ unSection s


instance SM.Updatable T.Text (Maybe T.Text) XDGDesktop where
    update key op xdg = case SM.keyPop key of
        (Nothing, k) ->
            (Nothing, xdg /*. (Nothing, updateSection k $ newValue <$> op))
        (Just key', k1) -> case SM.keyPop key' of
            (Nothing, k2) ->
                (Nothing, xdg /*. (Just k1, updateSection k2 $ newValue <$> op))
            _ -> (Just "triple nested keys or more is not supported", xdg)
      where
        updateSection _  SM.Erase   _        = Nothing
        updateSection k' (SM.Add v) Nothing  = Just (mempty /** (k', [v]))
        updateSection k' (SM.Add v) (Just s) = Just (s /** (k', [v]))
        updateSection k' (SM.Remove v) s =
            (/**. (k', fmap (L.filter (v /=)))) <$> s
        updateSection k' (SM.Replace v) s = (/**. (k', \_ -> Just [v])) <$> s
        updateSection k' SM.Enable s =
            (/**. (k', \v -> fmap (fmap (setEnabled True)) v)) <$> s
        updateSection k' SM.Disable s =
            (/**. (k', \v -> fmap (fmap (setEnabled False)) v)) <$> s
