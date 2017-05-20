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
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TemplateHaskell       #-}

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

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse

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
instance Hashable Int String where hashWithSalt = SH.hashWithSalt


data Prehashed h a = Prehashed {
    lazyHash :: !h
  , prehashedValue :: a
  }

newtype LazilyHashableFunction h a b = LHF {
    getLHF :: Prehashed h (a->b)
  }

type Hash' h = (Hashable h h, Hashable h String, Num h)

instance Hash' h => Category (LazilyHashableFunction h) where
  id = lhf defaultSalt id
  LHF (Prehashed hf f) . LHF (Prehashed hg g)
       = lhf (hashWithSalt hf hg) (f . g)

lhf :: h -> (a->b) -> LazilyHashableFunction h a b
lhf h = LHF . Prehashed h


-- | Transform an ordinary value into a pre-hashed one. This hashes the /source code
--   contained in the quasi quote/, assuming that the behaviour of anything invoked
--   therein will never change.
-- 
--   Applying this to anything but named, fixed-predefined values (standard library
--   functions etc.) is probably a bad idea.
fundamental :: QuasiQuoter
fundamental = QuasiQuoter (return . fund) undefined undefined undefined
 where fund v = AppE (AppE (VarE 'Prehashed)
                 (LitE . IntegerL . fromIntegral $ (hash v :: Int)))
                 vParsed
        where vParsed = case parseExp v of
                  Right exp -> exp
                  Left perr -> error perr
