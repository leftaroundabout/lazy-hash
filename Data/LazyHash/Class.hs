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

class Hash h where
  defaultSalt :: h
instance Hash Int where
  defaultSalt = -2578643520546668380
    -- same as http://hackage.haskell.org/package/hashable-1.2.6.0/docs/src/Data-Hashable-Class.html#hashWithSalt
    -- (on 64-bit)

infixl 6 #
class Hash h => Hashable h a where
  -- | Aka @hashWithSalt@.
  (#) :: h -> a -> h

  hash :: a -> h
  hash = (#) defaultSalt

instance Hashable Int Int where (#) = SH.hashWithSalt
instance Hashable Int String where (#) = SH.hashWithSalt


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
fundamental = QuasiQuoter (return . fund) undefined undefined undefined
 where fund v = AppE (AppE (VarE 'Prehashed)
                 (LitE . IntegerL . fromIntegral $ (hash v :: Int)))
                 vParsed
        where vParsed = case parseExp v of
                  Right exp -> exp
                  Left perr -> error perr
