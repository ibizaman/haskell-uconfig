{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : Syntax
Description : Generic syntax for config file
Copyright   : (c) Pierre Penninckx, 2020
License     : GPL-3
Maintainer  : Pierre Penninckx (ibizapeanut@gmail.com)
Stability   : experimental
Portability : POSIX

Datatypes representing a config file with sections, assignments and
comments. And functions to modify those datatypes. Its goal is to
retain all formatting while allowing the easiest transformation of its
content.

This module does not provide parsers to transform an actual config
file into this datatype, or to do the inverse transform. See
'Syntax.XDGDesktop.parser' and 'Syntax.XDGDesktop.generate' for that.

A few noteworthy features and conventions:

- All comments above an assignment belongs as 'preComments' to that
  assignment.
- All comments to the right of an assignment belongs as 'postComments'
  to that assignment.
- An assignment can be 'enabled' or not. It is considered disabled if
  it is preceded by a comment sign.
-}
module Syntax
    ( XDGDesktop(..)

    -- * Sections
    , Sections
    , newSections
    , unSections

    -- * Section
    , Section
    , newSection
    , unSection
    , (/*)
    , (/*?)
    , (/*.)
    -- * Value
    , Value(value, enabled, preComments, postComments)
    , newValue
    , setEnabled
    , (/**)
    , (/**?)
    , (/**.)

    -- * Comment
    , Comment(unComment)
    , newComment
    , newComment1
    , (<#)
    , (#>)

    -- * Query
    , getFirstSection
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


-- |Datatype representing any config file fitting [Systemd's extended
-- XDGDesktop](https://www.freedesktop.org/software/systemd/man/systemd.syntax.html)
-- syntax. It retains all formatting and comments.
data XDGDesktop = XDGDesktop
    { firstSection :: Section     -- ^Global section, that is
                                  -- everything before the first
                                  -- header.
    , firstComments :: Comment    -- ^'Comment' from the global
                                  -- section. This would only be
                                  -- filled out if there are no
                                  -- assignments in the first section.
    , sections :: Sections        -- ^Sections with headers.
    , trailingComments :: Comment -- ^'Comment' appearing after the
                                  -- last assignment.
    }
    deriving (Show, Eq)

-- |Union of two 'XDGDesktop'.
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

-- |Adds or replaces a 'Section' in a 'XDGDesktop'. The first argument
-- of the tuple is a 'Maybe':
--
-- - A 'Just' means the 'Section' will be added to the 'sections' of
-- the 'XDGDesktop'.
-- - A 'Nothing' means the 'Section' will be added to the
-- 'firstSection' of the 'XDGDesktop'.
(/*) :: XDGDesktop -> (Maybe T.Text, Section) -> XDGDesktop
x /* (Just k, s) = x
    { sections = let (Sections s') = sections x in Sections $ OM.insert k s s'
    }
x /* (Nothing, s) = x { firstSection = s }

-- |Adds, replaces or deletes a 'Section' in a 'XDGDesktop'. The first
-- argument of the tuple is a 'Maybe':
--
-- - A 'Just' means the 'Section' will be added to the 'sections' of
-- the 'XDGDesktop'.
-- - A 'Nothing' means the 'Section' will be added to the
-- 'firstSection' of the 'XDGDesktop'.
--
-- If the second argument, the 'Section', is @== mempty@, then the
-- 'Section' is deleted. Otherwise, the 'Section' is added.
(/*?) :: XDGDesktop -> (Maybe T.Text, Section) -> XDGDesktop
x /*? (Just k, s) = x
    { sections = let (Sections ss) = sections x
                 in  Sections $ if s == mempty
                         then OM.alter (\_ -> Nothing) k ss
                         else OM.insert k s ss
    }
x /*? (Nothing, s) = x { firstSection = s }

-- |Alters a 'Section' in a 'XDGDesktop'. The first argument
-- of the tuple is a 'Maybe':
--
-- - A 'Just' means the 'Section' that will be altered is in the 'sections' of
-- the 'XDGDesktop'.
-- - A 'Nothing' means the first 'Section' will be altered .
--
-- The second argument to the tuple is the function that alters a
-- 'Section'. It should follow the same behavior as
-- 'OrderedMap.alter'.
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


-- |Header name to 'Section' pairs. Think of it as a list of tuples
-- with header names on the left and 'Section's on the right.
newtype Sections = Sections {
      unSections :: OM.OrderedMap T.Text Section
    }
    deriving (Show, Eq)

-- |Create 'Sections' from a list of pairs.
newSections :: [(T.Text, Section)] -> Sections
newSections = Sections . OM.fromList

-- |Merges the content of two sections.
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

(/**?) :: Section -> (T.Text, [Value T.Text]) -> Section
(Section s) /**? (k, v) = Section $ if v == mempty
    then OM.alter (\_ -> Nothing) k s
    else OM.insertWith (flip (<>)) k v s

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
                   , preComments  = mempty
                   , postComments = mempty
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


data Comment = Comment
    { unComment :: [T.Text]
    , commentPrefix :: T.Text
    }
    deriving (Show, Eq)

newComment :: T.Text -> [T.Text] -> Comment
newComment p u = Comment u p

newComment1 :: T.Text -> T.Text -> Comment
newComment1 p u = Comment [u] p

-- |Merge 'Comment' contents while keeping the first 'commentPrefix'.
instance Semigroup Comment where
    Comment { unComment = ua, commentPrefix = p } <> Comment { unComment = ub }
        = Comment { unComment = ua <> ub, commentPrefix = p }

instance Monoid Comment where
    mempty = Comment [] "#"

instance Data.String.IsString Comment where
    fromString s = Comment (T.pack <$> lines s) "#"


getFirstSection :: XDGDesktop -> Section
getFirstSection = firstSection

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
        updateSection k' SM.Erase   Nothing  = Just (mempty /**? (k', mempty))
        updateSection k' SM.Erase   (Just s) = Just (s /**? (k', mempty))
        updateSection k' (SM.Add v) Nothing  = Just (mempty /** (k', [v]))
        updateSection k' (SM.Add v) (Just s) = Just (s /** (k', [v]))
        updateSection k' (SM.Remove v) s =
            (/**. (k', fmap (L.filter (v /=)))) <$> s
        updateSection k' (SM.Replace v) s = (/**. (k', \_ -> Just [v])) <$> s
        updateSection k' SM.Enable s =
            (/**. (k', \v -> fmap (fmap (setEnabled True)) v)) <$> s
        updateSection k' SM.Disable s =
            (/**. (k', \v -> fmap (fmap (setEnabled False)) v)) <$> s
