{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Main (main) where

import Data.ByteString
import Data.Either.Extra
import Data.FileEmbed
import Data.Yaml
import Test.Tasty
import Test.Tasty.QuickCheck

import Travis.Meta

main :: IO ()
main = Test.Tasty.defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ fixtures
  , errorCases
  ]

fixtures :: TestTree
fixtures = testGroup "Fixtures"
  [ f "self" $(embedFile ".travis.yml") $(embedFile ".travis.meta.yml")
  , f "with matrix" $(embedFile "fixtures/multi-processed.yml") $(embedFile "fixtures/multi.yml")
  , f "without matrix" $(embedFile "fixtures/without-matrix-processed.yml") $(embedFile "fixtures/without-matrix.yml")
  , f "multi-ghc" $(embedFile "fixtures/multi-ghc-processed.yml") $(embedFile "fixtures/multi-ghc.yml")
  , f "stack" $(embedFile "fixtures/stack-processed.yml") $(embedFile "fixtures/stack.yml")
  ]
  where f = fixture

fixture :: String -> ByteString -> ByteString -> TestTree
fixture name metaBS processedBS = testProperty name $ once $ actual === processed
  where actual     = decodeEither metaBS
        processed  = preprocessYaml =<< decodeEither processedBS

errorCases :: TestTree
errorCases = testGroup "Error cases"
  [ c "matrix.include specified" $(embedFile "fixtures/matrix-include.yml")
  ]
  where c name bs = testProperty name $ once $ isLeft $ preprocess bs
