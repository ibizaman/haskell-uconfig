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
import qualified Syntax
import qualified Syntax.XDGDesktop             as XDGDesktop
import qualified SyntaxModifier                as SM
import qualified Text.Nicify                   as Nicify
import qualified Parser                        as P


data Arguments = Arguments Bool FileType T.Text SM.ConstructResult

data FileType
    = FTGeneric
    | FTSystemdService


main :: IO ()
main = arguments >>= \case
    Arguments debug filetype file constructResult ->
        withSyntaxModifier constructResult $ \syntaxModifier ->
            parseXDG file $ \xdg -> do
                when debug $ do
                    putStr "Updates: "
                    print syntaxModifier
                    putStrLn ""

                updateXDG syntaxModifier xdg debug $ \updated ->
                    case filetype of
                        FTGeneric ->
                            putStrLn $ T.unpack $ XDGDesktop.generate updated
                        FTSystemdService -> roundtripParse xdg updated
  where
    withSyntaxModifier constructResult onSuccess =
        case SM.constructErrors constructResult of
            []   -> onSuccess $ SM.constructResult constructResult
            errs -> do
                putStrLn "Some UPDATEs could not be parsed:"
                sequence_ $ print <$> errs

    parseXDG :: T.Text -> (Syntax.Lvl2Config -> IO ()) -> IO ()
    parseXDG file f = SIO.withFile (T.unpack file) SIO.ReadMode $ \handle ->
        (P.parse XDGDesktop.parser <$> (T.pack <$> SIO.hGetContents handle))
            >>= \case
                    Left (err :: P.Error Void) ->
                        putStrLn $ "Error while parsing file: " <> show err
                    Right xdg -> f xdg

    updateXDG constructResult xdg debug onSuccess =
        case SM.apply constructResult xdg of
            (Just err, _) -> do
                putStrLn "An error was encountered while updating:"
                putStrLn . T.unpack $ err
            (Nothing, updated) -> do
                when debug $ do
                    putStr "Intermediate Representation: "
                    putStrLn $ Nicify.nicify $ show updated
                    putStrLn ""
                onSuccess updated

    roundtripParse parsed updated = case C.parser updated of
        C.ParseError err -> putStrLn $ "Error during update: " <> show err
        C.ParseSuccess (parsed' :: SystemdService) ->
            putStrLn
                $                            T.unpack
                $                            XDGDesktop.generate
                $                            C.unparser parsed'
                `XDGDesktop.followOrderFrom` parsed

arguments :: IO Arguments
arguments = Args.execParser $ Args.info
    (         Arguments
    <$>       Args.switch
                  (  Args.long "debug"
                  <> Args.short 'd'
                  <> Args.help
                         "Enable debug output (shows intended updates if any and internal representation)"
                  )
    <*>       Args.option
                  (Args.parsecArg
                      (P.choice
                          [ "systemdservice" $> FTSystemdService
                          , "generic" $> FTGeneric
                          ] :: P.Parser Void FileType
                      )
                  )
                  (  Args.metavar "FILETYPE"
                  <> Args.long "type"
                  <> Args.short 't'
                  <> Args.completeWith ["systemdservice", "generic"]
                  <> Args.value FTGeneric
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
