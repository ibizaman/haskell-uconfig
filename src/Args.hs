{-|
Module      : Args
Description : Helpers and only import needed for argument parsing
Copyright   : (c) Pierre Penninckx, 2020
License     : GPL-3
Maintainer  : Pierre Penninckx (ibizapeanut@gmail.com)
Stability   : experimental
Portability : POSIX

On top of re-exports, this module provides a few opinionated functions
that help with argument parsing.
-}
module Args
    (
      -- * Helpers
      subparser
    , desc
    , parsecArg

      -- * Re-exports from Options.Applicative
    , Args.execParser
    , Args.info
    , Args.argument
    , Args.strArgument
    , Args.command
    , Args.metavar
    , Args.helper
    , (Args.<**>)
    , Args.strOption
    , Args.flag
    , Args.switch
    , Args.long
    , Args.short
    , Args.help

      -- * Re-exports from Parser
    , CApp.some
    )
where

import qualified Control.Applicative           as CApp
import qualified Data.Text                     as T
import qualified Options.Applicative           as Args
import qualified Parser                        as P
import           Utils                          ( mapLeft )


-- |Returns a 'Args.Parser' for sub-commands from a list of
-- ('command', 'description', 'parser') with 'command' the sub-parser
-- name as it must be entered on the command-line, 'description' a
-- long description of the 'command' as it will show up in the command
-- help, and finally the 'parser' for that command.
subparser :: Foldable t => t (String, String, Args.Parser a) -> Args.Parser a
subparser commands = Args.hsubparser $ foldl
    (\xs (c, d, t) -> xs <> Args.command c (Args.info t (desc d)))
    mempty
    commands

-- |Set description of command-line help.
--
--     Args.info parser (desc "myprogram")
desc :: String -> Args.InfoMod a
desc d = Args.fullDesc <> Args.progDesc d

-- |Provides a 'Args.ReadM' from a Megaparsec 'P.Parser'.
parsecArg :: P.ShowErrorComponent e => P.Parser e v -> Args.ReadM v
parsecArg parser = Args.eitherReader (mapLeft show . P.parse parser . T.pack)
