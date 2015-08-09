{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Main (main) where

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
  , withoutMatrix
  , errorCases
  ]

selfTest :: TestTree
selfTest = testProperty "Self test" $ once $ actual === processed
  where actual     = decodeEither $(embedFile ".travis.yml")
        processed  = preprocessYaml =<< decodeEither $(embedFile ".travis.meta.yml")

withoutMatrix :: TestTree
withoutMatrix = testProperty "Self test without matrix" $ once $ actual === processed
  where actual     = decodeEither $(embedFile "fixtures/without-matrix-processed.yml")
        processed  = preprocessYaml =<< decodeEither $(embedFile "fixtures/without-matrix.yml")

errorCases :: TestTree
errorCases = testGroup "Error cases"
  [ c "matrix.include specified" $(embedFile "fixtures/matrix-include.yml")
  ]
  where c name bs = testProperty name $ once $ isLeft $ preprocess bs
