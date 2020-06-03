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
    , Sections(..)
    , Section(..)
    , Value(..)
    , Comment(..)
    , (/*)
    , (/**)
    , (<#)
    , (#>)
    )
where


import qualified Data.Map.Strict               as Map
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


newtype Sections = Sections (Map.Map T.Text Section)
    deriving (Show, Eq)

instance Semigroup Sections where
    Sections a <> Sections b = Sections $ a <> b

instance Monoid Sections where
    mempty = Sections Map.empty


newtype Section = Section (Map.Map T.Text Value)
    deriving (Show, Eq)

instance Semigroup Section where
    Section a <> Section b = Section $ a <> b

instance Monoid Section where
    mempty = Section Map.empty

(/**) :: Section -> (T.Text, Value) -> Section
(Section s) /** (k, v) = Section $ Map.insert k v s


data Value = Value
    { value :: T.Text
    , preComments :: Comment
    , postComments :: Comment
    }
    deriving (Show, Eq)

instance Data.String.IsString Value where
    fromString s =
        Value { value = T.pack s, preComments = mempty, postComments = mempty }

(<#) :: Value -> Comment -> Value
v <# c = v { preComments = c }

(#>) :: Value -> Comment -> Value
v #> c = v { postComments = c }


newtype Comment = Comment [T.Text]
    deriving (Show, Eq)

instance Semigroup Comment where
    Comment a <> Comment b = Comment $ a <> b

instance Monoid Comment where
    mempty = Comment []

instance Data.String.IsString Comment where
    fromString s = Comment $ T.pack <$> lines s
