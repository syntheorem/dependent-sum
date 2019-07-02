{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes      #-}
module Data.Dependent.Sum.Identity
    ( DSum
    , pattern (:=>)
    , module Data.Dependent.Sum
    ) where

import Data.Functor.Identity

import Data.Dependent.Sum hiding (DSum((:=>)), (==>))
import qualified Data.Dependent.Sum as DSum

-- | A 'DSum.DSum' specialized to 'Identity'.
type DSum tag = DSum.DSum tag Identity

-- | Pattern match on the value of a 'DSum'.
infixr 1 :=>
pattern (:=>) :: tag a -> a -> DSum tag
pattern tag :=> a = (DSum.:=>) tag (Identity a)
{-# COMPLETE (:=>) #-}
