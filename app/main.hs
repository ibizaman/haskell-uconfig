{-# LANGUAGE LambdaCase #-}
module Main
    ( main
    )
where

import qualified Config                        as C
import           Config.SystemdService          ( SystemdService(..) )
import qualified Data.Text                     as T
import qualified Args
import qualified System.IO                     as SIO
import qualified Text.Nicify                   as Nicify


data Arguments = ArgParse Config T.Text
               | ArgPrint Config T.Text

data Config = CSystemdService


main :: IO ()
main = arguments >>= \case
    ArgParse CSystemdService file -> parse file $ \parsed ->
        putStrLn $ Nicify.nicify $ show (parsed :: SystemdService)
    ArgPrint CSystemdService file -> parse file $ \parsed ->
        putStrLn $ T.unpack $ C.printer (parsed :: SystemdService)
  where
    parse file f = SIO.withFile (T.unpack file) SIO.ReadMode $ \handle ->
        (C.parse C.parser <$> (T.pack <$> SIO.hGetContents handle)) >>= \case
            Left err -> putStrLn $ "Error while parsing file: " <> T.unpack err
            Right parsed -> f parsed


arguments :: IO Arguments
arguments = Args.execParser $ Args.info
    (         Args.subparser
            [ ( "parse"
              , "Parse a file"
              , ArgParse <$> configtypeparser <*> Args.strArgument
                  (Args.metavar "FILE")
              )
            , ( "print"
              , "Parse a file and print it back"
              , ArgPrint <$> configtypeparser <*> Args.strArgument
                  (Args.metavar "FILE")
              )
            ]
    Args.<**> Args.helper
    )
    (Args.desc "uconfig")
  where
    configtypeparser = Args.subparser
        [ ( "systemdservice"
          , "Work on SystemdService files"
          , pure CSystemdService
          )
        ]
