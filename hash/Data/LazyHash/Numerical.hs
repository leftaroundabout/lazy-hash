-- |
-- Module      : Data.LazyHash.Numerical
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

module Data.LazyHash.Numerical where

import Data.LazyHash.Class

import qualified Data.Hashable as SH
import Data.Tagged

import Data.AffineSpace
import Data.VectorSpace



instance (Hash h, Hashable h x, AffineSpace x, Hashable h (Diff x))
             => AffineSpace (Prehashed h x) where
  type Diff (Prehashed h x) = Prehashed h (Diff x)   
  (.-.) = liftPH2 [fundamental|(.-.)|]
  (.+^) = liftPH2 [fundamental|(.+^)|]

instance (Hash h, Hashable h v, AdditiveGroup v) => AdditiveGroup (Prehashed h v) where
  zeroV = [fundamental|zeroV|]
  (^+^) = liftPH2 [fundamental|(^+^)|]
  negateV = liftPH [fundamental|negateV|]
instance (Hash h, Hashable h v, VectorSpace v) => VectorSpace (Prehashed h v) where
  type Scalar (Prehashed h v) = Prehashed h (Scalar v)
  (*^) = liftPH2 [fundamental|(*^)|]
instance (Hash h, Hashable h v, InnerSpace v, Hashable h (Scalar v))
           => InnerSpace (Prehashed h v) where
  (<.>) = liftPH2 [fundamental|(<.>)|]


instance (Hash h, Hashable h n, Num n) => Num (Prehashed h n) where
  fromInteger = strictHashed . fromInteger
  (+) = liftPH2 [fundamental|(+)|]
  (-) = liftPH2 [fundamental|(-)|]
  (*) = liftPH2 [fundamental|(*)|]
  negate = liftPH [fundamental|negate|]
  abs = liftPH [fundamental|abs|]
  signum = liftPH [fundamental|signum|]
instance (Hash h, Hashable h n, Fractional n) => Fractional (Prehashed h n) where
  fromRational = strictHashed . fromRational
  (/) = liftPH2 [fundamental|(/)|]
  recip = liftPH [fundamental|recip|]
instance (Hash h, Hashable h n, Floating n) => Floating (Prehashed h n) where
  logBase = liftPH2 [fundamental|logBase|]
  pi = [fundamental|pi|]
  exp = liftPH [fundamental|exp|]
  log = liftPH [fundamental|log|]
  sin = liftPH [fundamental|sin|]
  cos = liftPH [fundamental|cos|]
  asin = liftPH [fundamental|asin|]
  acos = liftPH [fundamental|acos|]
  tan = liftPH [fundamental|tan|]
  atan = liftPH [fundamental|atan|]
  sinh = liftPH [fundamental|sinh|]
  cosh = liftPH [fundamental|cosh|]
  tanh = liftPH [fundamental|tanh|]
  asinh = liftPH [fundamental|asinh|]
  acosh = liftPH [fundamental|acosh|]
  atanh = liftPH [fundamental|atanh|]
