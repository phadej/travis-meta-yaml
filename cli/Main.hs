{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString as BS
import           Data.FileEmbed (embedFile)
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Options.Applicative
import           Travis.Meta

data Command = GenerateCmd GenerateOpts
             | InitCmd
             | ShellScript String
             | LanguageTemplate String
  deriving (Eq, Show)

data GenerateOpts = GenerateOpts
  { _source :: FilePath
  , _target :: FilePath
  }
  deriving (Eq, Show)

generateParser :: Parser GenerateOpts
generateParser = GenerateOpts
  <$> strOption
      ( short 'i'
     <> long "input"
     <> metavar "SOURCE"
     <> value ".travis.meta.yml"
     <> showDefault
     <> help "Source, travis meta file" )
  <*> strOption
      ( short 'o'
     <> long "output"
     <> metavar "TARGET"
     <> value ".travis.yml"
     <> showDefault
     <> help "Target, travis yaml" )

shellScriptParser :: Parser Command
shellScriptParser =
  ShellScript <$> strArgument (metavar "SCRIPT")

langTemplateParser :: Parser Command
langTemplateParser =
  LanguageTemplate <$> strArgument (metavar "LANGUAGE")

commandParser :: Parser Command
commandParser = subparser $ mconcat
  [ command "generate"          (info (helper <*> (GenerateCmd <$> generateParser)) (progDesc "Generate .travis.yml file"))
  , command "init"              (info (helper <*> pure InitCmd) (progDesc "Initalise .travis.meta.yml and depdendencies"))
  , command "shell-script"      (info (helper <*> shellScriptParser) (progDesc "Show bundled shell script"))
  , command "language-template" (info (helper <*> langTemplateParser) (progDesc "Show bundled language template"))
  ]

execCommand :: Command -> IO ()
execCommand (GenerateCmd (GenerateOpts source target)) = preprocessIO source target
execCommand InitCmd =
  BS.writeFile ".travis.meta.yml" $(embedFile "data/multi-ghc.meta.yml")
execCommand (ShellScript name) =
  case lookup (T.pack name) shellScripts of
    Just script -> T.putStrLn script
    Nothing     -> mapM_ p shellScripts
  where p (name', _) = T.putStrLn $ "# " <> name'
execCommand (LanguageTemplate name) =
  case lookup (T.pack name) languageTemplates of
    Just template -> BS.putStr (encode' template)
    Nothing       -> mapM_ p languageTemplates
  where p (name', _) = T.putStrLn $ "# " <> name'

main :: IO ()
main = execParser opts >>= execCommand
  where
    opts = info (helper <*> commandParser)
      ( fullDesc
     <> header "travis-meta-yaml - .travis.yml preprocessor" )
