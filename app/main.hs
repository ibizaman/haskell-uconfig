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
import           Data.Functor                   ( ($>) )
import qualified Data.Text                     as T
import           Data.Void                      ( Void )
import qualified Args
import qualified System.IO                     as SIO
import qualified Syntax.XDGDesktop             as XDGDesktop
import qualified SyntaxModifier                as SM
import qualified Text.Nicify                   as Nicify
import qualified Parser                        as P


data Arguments = Arguments Bool FileType T.Text SM.ConstructResult

data FileType = FTSystemdService


main :: IO ()
main = arguments >>= \case
    Arguments debug FTSystemdService file constructResult ->
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
    <$>       Args.switch
                  (  Args.long "debug"
                  <> Args.short 'd'
                  <> Args.help
                         "Enable debug output (shows intended updates if any and internal representation)"
                  )
    <*>       Args.argument
                  (Args.parsecArg
                      (P.choice ["systemdservice" $> FTSystemdService] :: P.Parser
                            Void
                            FileType
                      )
                  )
                  (  Args.metavar "FILETYPE"
                  <> Args.completeWith ["systemdservice"]
                  <> Args.help
                         "File type of the file to update, currently only 'systemdservice' is the only one supported."
                  )
    <*>       Args.strArgument
                  (Args.metavar "FILENAME" <> Args.action "file" <> Args.help
                      "Path to file that will get modified."
                  )
    <*>       (SM.construct <$> Args.many
                  (Args.strArgument
                      (  Args.metavar "UPDATE..."
                      <> Args.help
                             "Updates of the form field-operator-value (ex: Unit.Description='my service')"
                      )
                  )
              )
    Args.<**> Args.helper
    )
    (Args.desc "Update config files while keeping formatting and comments.")
