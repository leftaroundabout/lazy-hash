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
  negateV = (<#>) [fundamental|negateV|]
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
  negate = (<#>) [fundamental|negate|]
  abs = (<#>) [fundamental|abs|]
  signum = (<#>) [fundamental|signum|]
instance (Hash h, Hashable h n, Fractional n) => Fractional (Prehashed h n) where
  fromRational = strictHashed . fromRational
  (/) = liftPH2 [fundamental|(/)|]
  recip = (<#>) [fundamental|recip|]
instance (Hash h, Hashable h n, Floating n) => Floating (Prehashed h n) where
  logBase = liftPH2 [fundamental|logBase|]
  pi = [fundamental|pi|]
  exp = (<#>) [fundamental|exp|]
  log = (<#>) [fundamental|log|]
  sin = (<#>) [fundamental|sin|]
  cos = (<#>) [fundamental|cos|]
  asin = (<#>) [fundamental|asin|]
  acos = (<#>) [fundamental|acos|]
  tan = (<#>) [fundamental|tan|]
  atan = (<#>) [fundamental|atan|]
  sinh = (<#>) [fundamental|sinh|]
  cosh = (<#>) [fundamental|cosh|]
  tanh = (<#>) [fundamental|tanh|]
  asinh = (<#>) [fundamental|asinh|]
  acosh = (<#>) [fundamental|acosh|]
  atanh = (<#>) [fundamental|atanh|]
