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
import           Data.Void                      ( Void )
import qualified Args
import qualified System.IO                     as SIO
import qualified Syntax.XDGDesktop             as XDGDesktop
import qualified SyntaxModifier                as SM
import qualified Text.Nicify                   as Nicify
import qualified Parser                        as P


data Arguments = Arguments Config Bool T.Text SM.ConstructResult

data Config = CSystemdService


main :: IO ()
main = arguments >>= \case
    Arguments CSystemdService debug file constructResult ->
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
                sequence_ $ print <$> errs
  where
    parseXDG :: T.Text -> (XDGDesktop.XDGDesktop -> IO ()) -> IO ()
    parseXDG file f = SIO.withFile (T.unpack file) SIO.ReadMode $ \handle ->
        (P.parse XDGDesktop.parser <$> (T.pack <$> SIO.hGetContents handle))
            >>= \case
                    Left (err :: P.Error Void) ->
                        putStrLn $ "Error while parsing file: " <> show err
                    Right parsed -> f parsed

arguments :: IO Arguments
arguments = Args.execParser $ Args.info
    (         Arguments
    <$>       configtypeparser
    <*>       Args.switch
                  (Args.long "debug" <> Args.short 'd' <> Args.help
                      "Enable debug output"
                  )
    <*>       Args.strArgument (Args.metavar "FILE")
    <*>       (   SM.construct
              <$> Args.many (Args.strArgument (Args.metavar "UPDATE..."))
              )
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
