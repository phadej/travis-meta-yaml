module Main (main) where

import Data.Monoid
import Options.Applicative
import Travis.Meta

data Command = GenerateCmd GenerateOpts
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

commandParser :: Parser Command
commandParser = subparser $ mconcat
  [ command "generate" (info (helper <*> (GenerateCmd <$> generateParser)) (progDesc "Generate .travis.yml file"))
  ]

execCommand :: Command -> IO ()
execCommand (GenerateCmd (GenerateOpts source target)) = preprocessIO source target

main :: IO ()
main = execParser opts >>= execCommand
  where
    opts = info (helper <*> commandParser)
      ( fullDesc
     <> header "travis-meta-yaml - .travis.yml preprocessor" )
