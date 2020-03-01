{-# LANGUAGE LambdaCase #-}
module Main
    ( main
    )
where

import qualified Config.SystemdService         as SystemdService
import qualified Data.Text                     as T
import qualified Options.Applicative           as Args
import qualified Parser                        as P
import qualified System.IO                     as SIO
import qualified Text.Nicify as Nicify


data Arguments = ArgParseSystemdService T.Text


main :: IO ()
main = arguments >>= \case
    ArgParseSystemdService file ->
        SIO.withFile (T.unpack file) SIO.ReadMode $ \handle ->
            (   P.parse SystemdService.parse
                <$> (T.pack <$> SIO.hGetContents handle)
                )
                >>= \case
                        Left err ->
                            putStrLn
                                $  "Error while parsing file: "
                                <> T.unpack err
                        Right parsed -> putStrLn $ Nicify.nicify $ show parsed


arguments :: IO Arguments
arguments = Args.execParser $ Args.info
    (         Args.hsubparser
            (Args.command
                "parse"
                (Args.info
                    (Args.hsubparser
                        (Args.command
                            "systemdservice"
                            (Args.info
                                (   ArgParseSystemdService
                                <$> Args.strArgument (Args.metavar "FILE")
                                )
                                mempty
                            )
                        )
                    )
                    (Args.fullDesc <> Args.progDesc "Parse a file")
                )
            )
    Args.<**> Args.helper
    )
    (Args.fullDesc <> Args.progDesc "uconfig")
