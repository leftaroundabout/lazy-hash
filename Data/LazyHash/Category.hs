-- |
-- Module      : Data.LazyHash.Category
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE QuasiQuotes           #-}

module Data.LazyHash.Category where

import Data.LazyHash.Class

import qualified Prelude as Hask hiding(foldl, sum, sequence)
import qualified Control.Applicative as Hask
import qualified Data.Foldable       as Hask
import Data.Foldable (all, elem, toList, sum, foldr1)

import Control.Category.Constrained.Prelude hiding
     ((^), all, elem, sum, forM, Foldable(..), foldr1, Traversable, traverse)
import Control.Arrow.Constrained
import Control.Monad.Constrained hiding (forM)

import qualified Data.Hashable as SH


instance Hash' h => Category (LazilyHashableFunction h) where
  id = [fundamental'|id|]
  LHF (Prehashed hf f) . LHF (Prehashed hg g)
       = lhf ([shash|.|] # hf # hg) (f . g)

lhf :: h -> (a->b) -> LazilyHashableFunction h a b
lhf h = LHF . Prehashed h

instance Hash' h => Cartesian (LazilyHashableFunction h) where
  swap = [fundamental'|swap|]
  attachUnit = [fundamental'|attachUnit|]
  detachUnit = [fundamental'|detachUnit|]
  regroup = [fundamental'|regroup|]
  regroup' = [fundamental'|regroup'|]

instance Hash' h
    => Functor (Prehashed h) (LazilyHashableFunction h) (LazilyHashableFunction h) where
  fmap (LHF (Prehashed hf f)) = lhf hff $ \(Prehashed hx x) -> Prehashed (hff # hx) $ f x
   where hff = [shash|fmap|] # hf
