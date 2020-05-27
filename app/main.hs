{-# LANGUAGE LambdaCase #-}
module Main
    ( main
    )
where

import qualified Config                        as C
import           Config.SystemdService          ( SystemdService(..) )
import qualified Data.Foldable                 as F
import qualified Data.Text                     as T
import qualified Args
import qualified System.IO                     as SIO
import qualified Text.Nicify                   as Nicify


data Arguments = ArgParse Config T.Text
               | ArgPrint Config T.Text
               | ArgGenerate Config C.FieldsTree
               | ArgWrite Config T.Text C.FieldsTree

data Config = CSystemdService


main :: IO ()
main = arguments >>= \case
    ArgParse CSystemdService file -> parse file $ \parsed ->
        putStrLn $ Nicify.nicify $ show (parsed :: SystemdService)
    ArgPrint CSystemdService file -> parse file $ \parsed ->
        putStrLn $ T.unpack $ C.printer (parsed :: SystemdService)
    ArgGenerate CSystemdService fieldsTree -> do
        let (errs, result) = C.generate fieldsTree
        printErrors errs
        case result of
            Nothing -> putStrLn "No valid config generated."
            Just content ->
                putStrLn $ T.unpack $ C.printer (content :: SystemdService)
    ArgWrite CSystemdService file fieldsTree -> do
        let (errs, result) = C.generate fieldsTree
        case errs of
            [] -> case result of
                Nothing      -> putStrLn "No valid config generated."
                Just content -> write file $ T.unpack $ C.printer
                    (content :: SystemdService)
            errs' -> printErrors errs'
  where
    parse file f = SIO.withFile (T.unpack file) SIO.ReadMode $ \handle ->
        (C.parse C.parser <$> (T.pack <$> SIO.hGetContents handle)) >>= \case
            Left err -> putStrLn $ "Error while parsing file: " <> T.unpack err
            Right parsed -> f parsed

    write file = SIO.writeFile (T.unpack file)

    printErrors :: [C.GenerateError] -> IO ()
    printErrors []   = return ()
    printErrors errs = do
        putStrLn "Got some errors while generating config:"
        F.sequenceA_ $ map print errs


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
            , ( "generate"
              , "Generate a file"
              , ArgGenerate
              <$> configtypeparser
              <*> (   C.fieldsTree
                  <$> (Args.some $ Args.argument
                          (Args.parsecArg C.pathValue)
                          (Args.metavar "PATHVALUE...")
                      )
                  )
              )
            , ( "write"
              , "Write a file"
              , ArgWrite
              <$> configtypeparser
              <*> Args.strArgument (Args.metavar "FILE")
              <*> (   C.fieldsTree
                  <$> (Args.some $ Args.argument
                          (Args.parsecArg C.pathValue)
                          (Args.metavar "PATHVALUE...")
                      )
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
