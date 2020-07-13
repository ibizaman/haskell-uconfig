module Args
    ( -- re-exports from Options.Applicative
      Args.execParser
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

      -- re-exports from Parser
    , CApp.some

      -- Helpers
    , subparser
    , desc
    , parsecArg
    )
where

import qualified Control.Applicative           as CApp
import qualified Data.Text                     as T
import qualified Options.Applicative           as Args
import qualified Parser                        as P


subparser :: Foldable t => t (String, String, Args.Parser a) -> Args.Parser a
subparser commands = Args.hsubparser $ foldl
    (\xs (c, d, t) -> xs <> Args.command c (Args.info t (desc d)))
    mempty
    commands

desc :: String -> Args.InfoMod a
desc d = Args.fullDesc <> Args.progDesc d

parsecArg :: P.Parser v -> Args.ReadM v
parsecArg parser = Args.eitherReader (P.parse parser . T.pack)
