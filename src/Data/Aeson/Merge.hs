-- |
-- Module      : Data.Aeson.Merge
-- Description : Simple 'Value' merge.
-- Copyright   : (c) Oleg Grenrus, 2005
-- License     : BSD3
-- Maintainer  : Oleg Grenrus <oleg.grenrus@iki.fi>
module Data.Aeson.Merge (merge) where

import Data.Aeson.Types
import Data.HashMap.Strict as HM

-- | A bit like <http://underscorejs.org/#extend _.extend>, but pure, left-bias and recursive.
merge :: Value -> Value -> Value
merge (Object a) (Object b) = Object (HM.unionWith merge a b)
merge a          _b         = a
