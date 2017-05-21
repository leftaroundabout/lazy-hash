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


import qualified Data.Hashable as SH

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse

import Data.Void

class Hash' h where
  defaultSalt :: h
instance Hash' Int where
  defaultSalt = -2578643520546668380
    -- same as http://hackage.haskell.org/package/hashable-1.2.6.0/docs/src/Data-Hashable-Class.html#hashWithSalt
    -- (on 64-bit)

infixl 6 #
class Hash' h => Hashable h a where
  -- | Aka @hashWithSalt@.
  (#) :: h -> a -> h

  hash :: a -> h
  hash = (#) defaultSalt

instance Hashable Int Int where (#) = SH.hashWithSalt
instance Hashable Int String where (#) = SH.hashWithSalt
instance Hashable Int () where (#) = SH.hashWithSalt
instance Hashable Int Void where (#) = SH.hashWithSalt


data Prehashed h a = Prehashed {
    lazyHash :: !h
  , prehashedValue :: a
  }

newtype LazilyHashableFunction h a b = LHF {
    getLHF :: Prehashed h (a->b)
  }

type Hash h = (Hashable h h, Hashable h String, Hashable h (), Hashable h Void, Num h)


-- | Compute the hash of a string at compile-time.
shash :: QuasiQuoter
shash = QuasiQuoter (return . ehash) undefined undefined undefined
 where ehash s = LitE . IntegerL $ fromIntegral (hash s :: Int)

-- | Transform an ordinary value into a pre-hashed one. This hashes the /source code
--   contained in the quasi quote/, assuming that the behaviour of anything invoked
--   therein will never change.
-- 
--   Applying this to anything but named, fixed-predefined values (standard library
--   functions etc.) is probably a bad idea.
fundamental :: QuasiQuoter

-- | 'fundamental' for single-argument functions (yields a 'LazilyHashableFunction'
--   instead of a 'Prehashed').
fundamental' :: QuasiQuoter

(fundamental:fundamental':_)
   = [ QuasiQuoter (return . wrap . fund) undefined undefined undefined
     | wrap <- iterate (AppE (ConE 'LHF) .) id ]
 where fund v = AppE (AppE (ConE 'Prehashed)
                 (LitE . IntegerL $ fromIntegral (hash v :: Int)))
                 vParsed
        where vParsed = case parseExp v of
                  Right exp -> exp
                  Left perr -> error perr




instance Hash h => Hashable h (Prehashed h a) where
  h₀ # Prehashed h _ = h₀ # h

instance Hash h => Hashable h (LazilyHashableFunction h a b) where
  h₀ # LHF (Prehashed h _) = h₀ # h


strictHashed :: Hashable h a => a -> Prehashed h a
strictHashed a = Prehashed (hash a) a

liftPH :: Hash h => Prehashed h (a->b) -> Prehashed h a -> Prehashed h b
liftPH (Prehashed hf f) (Prehashed ha a) = Prehashed (hf#ha) $ f a

liftPH2 :: Hash h => Prehashed h (a->b->c) -> Prehashed h a->Prehashed h b->Prehashed h c
liftPH2 (Prehashed hf f) (Prehashed ha a) (Prehashed hb b) = Prehashed (hf#ha#hb) $ f a b
