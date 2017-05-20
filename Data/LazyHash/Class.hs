-- |
-- Module      : Data.LazyHash.Class
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

module Data.LazyHash.Class where


import qualified Prelude as Hask hiding(foldl, sum, sequence)
import qualified Control.Applicative as Hask
import qualified Data.Foldable       as Hask
import Data.Foldable (all, elem, toList, sum, foldr1)

import Control.Category.Constrained.Prelude hiding
     ((^), all, elem, sum, forM, Foldable(..), foldr1, Traversable, traverse)
import Control.Arrow.Constrained
import Control.Monad.Constrained hiding (forM)

import qualified Data.Hashable as SH


class Hash h where
  defaultSalt :: h
instance Hash Int where
  defaultSalt = -2578643520546668380
    -- same as http://hackage.haskell.org/package/hashable-1.2.6.0/docs/src/Data-Hashable-Class.html#hashWithSalt
    -- (on 64-bit)

class Hash h => Hashable h a where
  hashWithSalt :: h -> a -> h

  hash :: a -> h
  hash = hashWithSalt defaultSalt

instance Hashable Int Int where hashWithSalt = SH.hashWithSalt


data LHashableFunction h a b = LHashableFunction {
    functionHash :: h
  , lhashableFunction :: a -> b
  }

type Hash' h = (Hashable h h, Hashable h String)

instance Hash' h => Category (LHashableFunction h) where
  id = LHashableFunction defaultSalt id
  LHashableFunction hf f . LHashableFunction hg g
       = LHashableFunction (hashWithSalt hf hg) (f . g)
