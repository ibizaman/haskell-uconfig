{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Main
    ( main
    )
where

import qualified Config                        as C
import           Config.SystemdService          ( SystemdService(..) )
import           Control.Monad                  ( when )
import qualified Data.Text                     as T
import qualified Args
import qualified System.IO                     as SIO
import qualified Syntax.XDGDesktop             as XDGDesktop
import qualified SyntaxModifier                as SM
import qualified Text.Nicify                   as Nicify
import qualified Parser                        as P


data Arguments = ArgParse Config T.Text
               | ArgPrint Config T.Text
               | ArgUpdate Config Bool T.Text SM.ConstructResult

data Config = CSystemdService


main :: IO ()
main = arguments >>= \case
    ArgParse CSystemdService file ->
        parse file $ \(parsed :: SystemdService) ->
            putStrLn $ Nicify.nicify $ show parsed
    ArgPrint CSystemdService file ->
        parse file $ \(parsed :: SystemdService) ->
            putStrLn $ T.unpack $ XDGDesktop.generate $ C.unparser parsed
    ArgUpdate CSystemdService debug file constructResult ->
        case SM.constructErrors constructResult of
            [] -> parseXDG file $ \parsed -> do
                when debug $ do
                    putStr "Updates: "
                    print $ SM.constructResult constructResult
                    putStrLn ""
                case SM.apply (SM.constructResult constructResult) parsed of
                    (Nothing, updated) -> do
                        when debug $ do
                            putStr "Intermediate Representation: "
                            putStrLn $ Nicify.nicify $ show updated
                            putStrLn ""
                        case C.parser updated of
                            C.ParseError err ->
                                putStrLn $ "Error during update: " <> show err
                            C.ParseSuccess (parsed' :: SystemdService) ->
                                putStrLn
                                    $ T.unpack
                                    $ XDGDesktop.generate
                                    $ C.unparser parsed'
                                    `XDGDesktop.followOrderFrom` parsed
                    (Just err, _) -> do
                        putStrLn "An error was encountered while updating:"
                        putStrLn . T.unpack $ err
            errs -> do
                putStrLn "Some UPDATEs could not be parsed:"
                sequence_ $ putStrLn . T.unpack <$> errs
  where
    parseXDG :: T.Text -> (XDGDesktop.XDGDesktop -> IO ()) -> IO ()
    parseXDG file f = SIO.withFile (T.unpack file) SIO.ReadMode $ \handle ->
        (P.parse XDGDesktop.parser <$> (T.pack <$> SIO.hGetContents handle))
            >>= \case
                    Left err ->
                        putStrLn $ "Error while parsing file: " <> show err
                    Right parsed -> f parsed

    parse
        :: (C.Config XDGDesktop.XDGDesktop v) => T.Text -> (v -> IO ()) -> IO ()
    parse file f = SIO.withFile (T.unpack file) SIO.ReadMode $ \handle ->
        (   fmap C.parser
            .   P.parse XDGDesktop.parser
            <$> (T.pack <$> SIO.hGetContents handle)
            )
            >>= \case
                    Left err ->
                        putStrLn $ "Error while parsing file: " <> show err
                    Right (C.ParseError err) ->
                        putStrLn $ "Error while parsing file: " <> show err
                    Right (C.ParseSuccess parsed) -> f parsed


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
            , ( "update"
              , "Update an existing config"
              , ArgUpdate
              <$> configtypeparser
              <*> Args.switch
                      (Args.long "debug" <> Args.short 'd' <> Args.help
                          "Enable debug output"
                      )
              <*> Args.strArgument (Args.metavar "FILE")
              <*> (SM.construct <$> Args.some
                      (Args.strArgument (Args.metavar "UPDATE..."))
                  )
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
