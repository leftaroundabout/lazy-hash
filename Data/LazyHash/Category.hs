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
{-# LANGUAGE TypeFamilies          #-}
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


instance Hash h => Category (LazilyHashableFunction h) where
  type Object (LazilyHashableFunction h) a = Hashable h a
  id = [fundamental'|id|]
  LHF (Prehashed hf f) . LHF (Prehashed hg g)
       = lhf ([shash|.|] # hf # hg) (f . g)

lhf :: h -> (a->b) -> LazilyHashableFunction h a b
lhf h = LHF . Prehashed h

instance Hash h => Cartesian (LazilyHashableFunction h) where
  swap = [fundamental'|swap|]
  attachUnit = [fundamental'|attachUnit|]
  detachUnit = [fundamental'|detachUnit|]
  regroup = [fundamental'|regroup|]
  regroup' = [fundamental'|regroup'|]

instance Hash h => Curry (LazilyHashableFunction h) where
  curry (LHF (Prehashed h f))
     = LHF $ Prehashed ([shash|curry|]#h) $ LHF . Prehashed 0 . curry f
  uncurry (LHF (Prehashed h f))
     = LHF . Prehashed h . uncurry $ prehashedValue . getLHF . f

instance Hash h
    => Functor (Prehashed h) (LazilyHashableFunction h) (LazilyHashableFunction h) where
  fmap (LHF (Prehashed hf f)) = lhf hff $ \(Prehashed hx x) -> Prehashed (hff # hx) $ f x
   where hff = [shash|fmap|] # hf

instance Hash h
    => Monoidal (Prehashed h) (LazilyHashableFunction h) (LazilyHashableFunction h) where
  pureUnit = LHF (Prehashed [shash|pureUnit|] (Prehashed $ hash()))
  fzipWith (LHF (Prehashed hf f)) = lhf hff
      $ \(Prehashed hx x, Prehashed hy y) -> Prehashed (hff # hx # hy) $ f (x,y)
   where hff = [shash|fzipWith|] # hf

instance Hash h
    => Applicative (Prehashed h) (LazilyHashableFunction h) (LazilyHashableFunction h) where
  pure = LHF . Prehashed [shash|pure|] $ \v -> Prehashed (hash v) v

instance Hash h => Monad (Prehashed h) (LazilyHashableFunction h) where
  join = LHF . Prehashed [shash|join|] $ \(Prehashed h (Prehashed i a))
                                      -> Prehashed ([shash|joined|] # h # i) a
