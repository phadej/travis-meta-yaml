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
  [ selfTest
  , withMatrix
  , withoutMatrix
  , errorCases
  ]

selfTest :: TestTree
selfTest = fixture "Self test" $(embedFile ".travis.yml") $(embedFile ".travis.meta.yml")

withMatrix :: TestTree
withMatrix = fixture "without matrix" $(embedFile "fixtures/multi-processed.yml") $(embedFile "fixtures/multi.yml")

withoutMatrix :: TestTree
withoutMatrix = fixture "without matrix" $(embedFile "fixtures/without-matrix-processed.yml") $(embedFile "fixtures/without-matrix.yml")

fixture :: String -> ByteString -> ByteString -> TestTree
fixture name metaBS processedBS = testProperty name $ once $ actual === processed
  where actual     = decodeEither metaBS
        processed  = preprocessYaml =<< decodeEither processedBS

errorCases :: TestTree
errorCases = testGroup "Error cases"
  [ c "matrix.include specified" $(embedFile "fixtures/matrix-include.yml")
  ]
  where c name bs = testProperty name $ once $ isLeft $ preprocess bs
