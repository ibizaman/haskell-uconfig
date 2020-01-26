{-|
Module      : Config
Description : Config file parsers
Copyright   : (c) Pierre Penninckx, 2020
License     : GPL-3
Maintainer  : Pierre Penninckx (ibizapeanut@gmail.com)
Stability   : experimental
Portability : POSIX

The Config module provides functions for parsing config files.
-}
module Config
    ( Config(..)
    , Assignment(..)
    , P.parse
    , flat
    , assignment
    )
where


import qualified Data.Text                     as T

import qualified Parser                        as P


newtype Config = Config [Assignment]
  deriving (Show, Eq)


data Assignment = Assignment T.Text T.Text
  deriving (Show, Eq)


flat :: P.Parser Config
flat = Config <$> P.newlineSeparated assignment


assignment :: P.Parser Assignment
assignment = Assignment <$> P.until '=' <*> P.line
