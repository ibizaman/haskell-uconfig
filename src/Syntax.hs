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
    , Value(value, preComments, postComments)
    , newValue
    , Comment(unComment)
    , newComment
    , (/*)
    , (/**)
    , (<#)
    , (#>)

    -- Query
    , getMainSection
    , getSection
    , getValue
    )
where


import qualified Data.Map.Strict               as Map
import qualified Data.Maybe                    as M
import qualified Data.String
import qualified Data.Text                     as T


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
    { sections = let (Sections s') = sections x in Sections $ Map.insert k s s'
    }
x /* (Nothing, s) = x { firstSection = s }


newtype Sections = Sections {
      unSections :: Map.Map T.Text Section
    }
    deriving (Show, Eq)

newSections :: Map.Map T.Text Section -> Sections
newSections = Sections

instance Semigroup Sections where
    Sections a <> Sections b = Sections $ a <> b

instance Monoid Sections where
    mempty = Sections Map.empty


newtype Section = Section {
      unSection :: Map.Map T.Text [Value T.Text]
    }
    deriving (Show, Eq)

newSection :: Map.Map T.Text [Value T.Text] -> Section
newSection = Section

instance Semigroup Section where
    Section a <> Section b = Section $ a <> b

instance Monoid Section where
    mempty = Section Map.empty

(/**) :: Section -> (T.Text, [Value T.Text]) -> Section
(Section s) /** (k, v) = Section $ Map.insertWith (flip (<>)) k v s


data Value v = Value
    { value :: v
    , preComments :: Comment
    , postComments :: Comment
    }
    deriving (Show, Eq)

newValue :: v -> Value v
newValue v =
    Value { value = v, preComments = Comment [], postComments = Comment [] }

instance Data.String.IsString v => Data.String.IsString (Value v) where
    fromString s = Value { value        = Data.String.fromString s
                         , preComments  = mempty
                         , postComments = mempty
                         }

instance Functor Value where
    fmap f v = v { value = f (value v) }

instance (Semigroup v) => Semigroup (Value v) where
    Value { value = v1, preComments = preC1, postComments = postC1 } <> Value { value = v2, preComments = preC2, postComments = postC2 }
        = Value { value        = v1 <> v2
                , preComments  = preC1 <> preC2
                , postComments = postC1 <> postC2
                }

instance (Monoid v) => Monoid (Value v) where
    mempty = newValue mempty

(<#) :: Value v -> Comment -> Value v
v <# c = v { preComments = c }

(#>) :: Value v -> Comment -> Value v
v #> c = v { postComments = c }


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
    M.fromMaybe mempty $ Map.lookup name $ unSections $ sections x

getValue :: Section -> T.Text -> [Value T.Text]
getValue s name = M.fromMaybe mempty $ Map.lookup name $ unSection s
