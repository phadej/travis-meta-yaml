module Main (main) where

import Travis.Meta
import Options.Applicative

data Opts = Opts
  { _source :: FilePath
  , _target :: FilePath
  }
  deriving (Eq, Show)

sample :: Parser Opts
sample = Opts
  <$> strArgument
      ( metavar "SOURCE"
     <> value ".travis.meta.yml"
     <> showDefault
     <> help "Source, travis meta file" )
  <*> strArgument
      ( metavar "TARGET"
     <> value ".travis.yml"
     <> showDefault
     <> help "Target, travis yaml" )

main :: IO ()
main = execParser opts >>= \(Opts source target) -> preprocessIO source target
  where
    opts = info (helper <*> sample)
      ( fullDesc
     <> header "travis-meta-yaml - .travis.yml preprocessor" )
