{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Travis.Meta (
  -- * High level
    preprocessIO
  , preprocess
  , preprocessYaml
  -- * Internal
  , Env
  , parseEnv
  , interpolateEnv
  ) where

import Control.Lens
import Control.Monad hiding (sequence)
import Data.Aeson.Types
import Data.Aeson.Lens
import Data.ByteString as BS
import Data.Char
import Data.Maybe
import Data.Monoid
import Data.Text as T
import Data.Traversable
import Data.Yaml
import Prelude hiding (sequence)
import Text.Regex.Applicative

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
interpolationRe :: (String -> Maybe String) -> RE Char (Maybe String)
interpolationRe l = comb <$> many (interpolationChar l)
  where comb :: [Maybe [a]] -> Maybe [a]
        comb = fmap Prelude.concat . sequence

interpolationChar :: (String -> Maybe String) -> RE Char (Maybe String)
interpolationChar l = var <|> other
  where var = l <$ sym '$' <*> many (psym isAlpha)
        other = (\x -> Just [x]) <$> anySym

-- | Interpolate env. Substitute all @$VAR@ occurrences with values from 'Env'.
-- If variable is not in the environment, return 'Nothing'.
--
-- > >>> interpolateEnv [("FOO", "foo")] "res-$FOO-bar"
-- > Just "res-foo-bar"
--
-- > >>> interpolateEnv [("FOO","foo")] "yes-$FOOBAR-$FOO"
-- > Nothing
interpolateEnv :: Env -> Text -> Either String Text
interpolateEnv env = hoist . fmap T.pack . join . match (interpolationRe l) . T.unpack
  where l = fmap T.unpack . flip lookup env . T.pack

hoist :: Maybe a -> Either String a
hoist Nothing = Left "error in interpolation"
hoist (Just a) = Right a

preprocessYaml :: Value -> Either String Value
preprocessYaml v = do
  assertNoMatrixInclude v
  matrixInclude <- buildMatrixInclude v
  let v' = v & _Object . at "env" .~ Nothing
             & _Object . at "addons" .~ Nothing
             & _Object . at "matrix" ?~ (fromMaybe (Object mempty) (v ^? key "matrix"))
             & key "matrix" . _Object . at "include" ?~ matrixInclude
  return v'

buildMatrixInclude :: Value -> Either String Value
buildMatrixInclude v = toJSON <$> mk `traverse` envs
  where addons  = v ^? key "addons"
        envs    = v ^.. key "env" . values . _String
        mk env  = do env' <- parseEnv env
                     addons' <- traverseOf (deep _String) (interpolateEnv env') `traverse` addons
                     case addons' of
                       Just addons'' -> return $ object [ "env" Data.Yaml..= env, "addons" Data.Yaml..= addons'' ]
                       Nothing       -> return $ object [ "env" Data.Yaml..= env ]
                     

assertNoMatrixInclude :: Value -> Either String ()
assertNoMatrixInclude v =
  case v ^? key "matrix" . key "include" of
    Nothing -> Right ()
    Just v' -> Left $ "matrix.include specified: " ++ show v'

preprocess :: ByteString -> Either String ByteString
preprocess = fmap encode . preprocessYaml <=< decodeEither

preprocessIO :: FilePath -> FilePath -> IO ()
preprocessIO source target = do
  contents <- BS.readFile source
  case preprocess contents of
    Left err -> error err
    Right bs -> BS.writeFile target bs

-- Right v <- decodeEither <$> BS.readFile ".travis.meta.yml"  :: IO (Either String Value)
-- BS.putStr $ encode $ preprocessYaml v
