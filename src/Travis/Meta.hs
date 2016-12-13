{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      : Travis.Meta
-- Description : Travis preprocessor
-- Copyright   : (c) Oleg Grenrus, 2005
-- License     : BSD3
-- Maintainer  : Oleg Grenrus <oleg.grenrus@iki.fi>
module Travis.Meta (
  -- * High level
    preprocessIO
  , preprocess
  , preprocessYaml
  -- * Internal
  , Env
  , parseEnv
  , interpolateEnv
  , unlinesShell
  , shellScripts
  , languageTemplates
  , encode'
  ) where

import Control.Category hiding ((.))
import Control.Arrow (second)
import Control.Lens hiding ((.=))
import Control.Monad hiding (sequence)
import Data.Aeson.Lens
import Data.Aeson.Merge
import Data.Aeson.Types
import Data.ByteString as BS
import Data.Char
import Data.FileEmbed
import Data.Foldable
import Data.Function (on)
import Data.List as L (map, elemIndex, filter)
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Text as T
import Data.Traversable
import Data.Vector.Lens (vector)
import Data.Version
import Data.Yaml
import Data.Yaml.Pretty
import Prelude hiding (sequence)
import Text.Regex.Applicative.Text as RE

import qualified Paths_travis_meta_yaml as Meta

type Env = [(Text, Text)]

-- | Parse environment string.
--
-- > >>> parseEnv "CABALVER=1.18 GHCVER=7.8.4"
-- > Right [("CABALVER","1.18"),("GHCVER","7.8.4")]
parseEnv :: Text -> Either String Env
parseEnv = traverse (f . T.splitOn "=") . T.words
  where f [k, n] = Right (k, n)
        f _      = Left "Cannot parse"

-- > match (interpolationRe $ flip lookup [("foo", "bar")]) "$foo"
-- Just (Just "bar")
interpolationRe :: (Text -> Maybe Text) -> RE' (Maybe Text)
interpolationRe l = comb <$> many (interpolationChar l)
  where comb :: [Maybe Text] -> Maybe Text
        comb = fmap T.concat . sequence

isVarChar :: Char -> Bool
isVarChar = (||) <$> isAlpha <*> (=='_')

interpolationChar :: (Text -> Maybe Text) -> RE' (Maybe Text)
interpolationChar l = var <|> other
  where var = l . T.pack <$ sym '$' <*> many (psym isVarChar)
        other = Just . T.singleton <$> anySym

-- | Interpolate env. Substitute all @$VAR@ occurrences with values from 'Env'.
-- If variable is not in the environment, return 'Nothing'.
--
-- > >>> interpolateEnv [("FOO", "foo")] "res-$FOO-bar"
-- > Just "res-foo-bar"
--
-- > >>> interpolateEnv [("FOO","foo")] "yes-$FOOBAR-$FOO"
-- > Nothing
interpolateEnv :: Env -> Text -> Maybe Text
interpolateEnv env = join . match (interpolationRe l)
  where l = flip lookup env

-- | Like 'interpolateEnv' but substitue non-existing variables with empty string.
--
-- > >>> interpolateEnv [("FOO","foo")] "yes-$FOOBAR-$FOO"
-- > "yes--foo"
interpolateEnv' :: Env -> Text -> Text
interpolateEnv' env = RE.replace (f <$ sym '$' <*> many (psym isVarChar))
  where f :: String -> Text
        f = fromMaybe "" . flip lookup env . T.pack

preprocessYaml :: Value -> Either String Value
preprocessYaml = preprocessYaml' . processMeta . processLanguage

processMeta :: Value -> Value
processMeta v = v'''
  where
    v' = v & _Object . at "meta" .~ Nothing
    v'' = case (v ^? key "meta" . key "pre") of
      Just meta -> merge meta v'
      Nothing   -> v'
    v''' = case (v ^? key "meta" . key "post") of
      Just meta -> merge v'' meta
      Nothing   -> v''

preprocessYaml' :: Value -> Either String Value
preprocessYaml' v = do
  assertNoMatrixInclude v
  matrixInclude <- buildMatrixInclude v
  let v' = v & deep _String %~ embedShellScripts
             & _Object . at "env" .~ Nothing
             & _Object . at "addons" .~ Nothing
             & _Object . at "compiler" .~ Nothing
             & _Object . at "meta" .~ Nothing
             & _Object . at "matrix" ?~ (fromMaybe (Object mempty) (v ^? key "matrix"))
             & key "matrix" . _Object . at "include" ?~ matrixInclude
  return v'

processLanguage :: Value -> Value
processLanguage v =
  case (v ^? key "language" . _String) >>= flip lookup languageTemplates of
    Just template -> merge (v & _Object . at "language" .~ Nothing) template
    Nothing       -> v

buildMatrixInclude :: Value -> Either String Value
buildMatrixInclude v = toJSON <$> mk `traverse` envs
  where addons   = v ^? key "addons"
        compiler = v ^? key "compiler" . _String
        envs     = v ^.. key "env" . values . _String
        mk env   = do env' <- parseEnv env
                      let interpolate = traverseOf _String (interpolateEnv env')
                          addons'     = addons & _Just . key "apt" . key "packages" . _Array . from vector %~ mapMaybe interpolate
                          compiler'   = compiler & _Just %~ T.strip . interpolateEnv' env'
                      return $ object $ catMaybes
                        [ Just $ "env" .= env
                        , ("addons" .=) <$> addons'
                        , ("compiler" .=) <$> compiler'
                        ]

assertNoMatrixInclude :: Value -> Either String ()
assertNoMatrixInclude v =
  case v ^? key "matrix" . key "include" of
    Nothing -> Right ()
    Just v' -> Left $ "matrix.include specified: " ++ show v'

header :: ByteString
header = "# This file has been generated by travis-meta-yaml " <> fromString (showVersion Meta.version) <> "\n# see https://github.com/phadej/travis-meta-yaml\n"

preprocess :: ByteString -> Either String ByteString
preprocess = fmap ((header <>) . encode') . preprocessYaml <=< decodeEither

preprocessIO :: FilePath -> FilePath -> IO ()
preprocessIO source target = do
  contents <- BS.readFile source
  case preprocess contents of
    Left err -> error err
    Right bs -> BS.writeFile target bs

-- | name and contents pairs
shellScripts :: [(Text, Text)]
shellScripts =
  [ ("multi-ghc-install.sh", $(embedStringFile "data/multi-ghc-install.sh"))
  ]

languageTemplates :: [(Text, Value)]
languageTemplates =
  [ t "haskell-stack" $(embedFile "data/stack.yml")
  , t "haskell-multi-ghc" $(embedFile "data/multi-ghc.yml")
  ]
  where t name bs = (name, fromJust' (T.unpack name) $ decode bs)

embedShellScripts :: Text -> Text
embedShellScripts = appEndo $ foldMap (Endo . uncurry embedShellFile . second unlinesShell) shellScripts

fromJust' :: String -> Maybe a -> a
fromJust' _ (Just x) = x
fromJust' e Nothing  = error $ "fromJust: Nothing -- " <> e

embedShellFile :: Text -> Text -> Text -> Text
embedShellFile filename contents =
    RE.replace (contents <$ string "sh" <* some (sym ' ') <* string filename)

unlinesShell :: Text -> Text
unlinesShell = T.lines >>>
               L.map (strip . stripComments) >>>
               L.filter (not . T.null) >>>
               L.map (fixSemiColonThenElse . (<> ";")) >>>
               T.intercalate " "
  where stripComments = RE.replace ("" <$ sym '#' <* many anySym)
        fixSemiColonThenElse = RE.replace ((string "then" <|> string "else") <* sym ';')

-- Right v <- decodeEither <$> BS.readFile ".travis.meta.yml"  :: IO (Either String Value)
-- BS.putStr $ encode $ preprocessYaml v

-- Nothing is smaller then Just
-- We also swap params.
elemIndexE :: Eq a => [a] -> a -> Either Int ()
elemIndexE l e = maybe (Right ()) Left (L.elemIndex e l)

listCompare :: Eq a => [a] -> a -> a -> Ordering
listCompare l = compare `on` elemIndexE l

preferredOrder :: [Text]
preferredOrder = ["sudo", "language", "before_install", "install", "script", "matrix"]

encode' :: ToJSON a => a -> ByteString
encode' = encodePretty cfg
  where cfg = setConfCompare (listCompare preferredOrder) defConfig
