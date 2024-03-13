{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LDH.CLI.Command
  ( Command(..)
  , options
  , runCommand
  ) where

import Protolude

import Options.Applicative (Parser, metavar, strArgument, long, short, help, subparser, command, info, progDesc, ParserInfo, fullDesc, header, flag')
import System.Directory (doesDirectoryExist)

data Command
  = Upload UploadOptions

data UploadOptions = UploadOptions
  { fileOrDirectory :: FilePath
  , uoDeleteAfterUpload :: Maybe Bool
  } deriving (Show)

deleteAfterFlag :: Parser Bool
deleteAfterFlag = flag' True (long "delete-after-upload" <> short 'd' <> help "Whether to delete the uploaded file")

keepFlag :: Parser Bool
keepFlag = flag' True (long "keep-after-upload" <> short 'k' <> help "Don not delete a file after upload")

deleteParser :: Parser (Maybe Bool)
deleteParser = optional $ deleteAfterFlag <|> keepFlag

uploadCommand :: Parser Command
uploadCommand = Upload <$>
  ( UploadOptions
    <$> strArgument (metavar "FILE-OR-DIR" <> help "The file or directory to upload")
    <*> deleteParser)

commandParser :: Parser Command
commandParser = subparser $
  command "upload" (info uploadCommand (progDesc "Upload the given file or the files in the given directory."))

options :: ParserInfo Command
options = info commandParser
  ( fullDesc
 <> progDesc "A command-line interface for a rainbow-hash server."
 <> header "A header for the CLI for rainbow-hash."
  )

runCommand
  :: Command
  -> IO ()
runCommand cmd = do
  case cmd of
    Upload (UploadOptions fileOrDirectory' shouldDelete) -> do
      isDir <- doesDirectoryExist fileOrDirectory'
      if isDir
        then putStrLn $ "Uploading directory " <> fileOrDirectory'
        else putStrLn $ "Uploading file " <> fileOrDirectory'
