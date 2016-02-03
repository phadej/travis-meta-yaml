-- |
-- Module      : Data.Aeson.Merge
-- Description : Simple 'Value' merge.
-- Copyright   : (c) Oleg Grenrus, 2005
-- License     : BSD3
-- Maintainer  : Oleg Grenrus <oleg.grenrus@iki.fi>
module Data.Aeson.Merge (merge) where

import Data.Aeson (Value)
import qualified Data.Aeson.Extra.Merge as M
import Data.Monoid ((<>))
import Data.HashMap.Strict as HM

-- | A bit like <http://underscorejs.org/#extend _.extend>, but pure, left-bias and recursive.
merge :: Value -> Value -> Value
merge = M.merge f
  where f r (M.ObjectF a) (M.ObjectF b) = M.ObjectF $ HM.unionWith r a b
        f _ (M.ArrayF a)  (M.ArrayF b)  = M.ArrayF $ a <> b
        f _  a            _             = a