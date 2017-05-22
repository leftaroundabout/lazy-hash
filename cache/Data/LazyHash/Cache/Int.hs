-- |
-- Module      : Data.LazyHash.Cache.Int
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 
-- Identical interface to "Data.LazyHash.Cache", but specialised to hashes of type 'Int'
-- (which are computed with "Data.Hashable"). At only 64 bits, collisions are
-- <https://en.wikipedia.org/wiki/Birthday_problem likely> with this type once you store
-- a considerable number of values, so don't use it for serious applications.

{-# LANGUAGE LambdaCase #-}

module Data.LazyHash.Cache.Int (
             -- * The caching actions
               cached, cachedTmp, cachedWithin
             -- * Prehashing tools
             , fundamental, (<#>), liftPH2
                               ) where

import Data.LazyHash.Cache (cachedValueInFile)

import Data.LazyHash.Class
import Data.LazyHash.Numerical ()

import qualified Data.Hashable as SH

import Data.Binary
import System.FilePath
import System.Directory

import Data.Typeable

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BS (toStrict)

import Numeric (showHex) 
import Data.Word (Word64) 

cached :: (Binary a, Typeable a) => Prehashed Int a -> IO a
cached = cachedWithin ".hscache/lazy-hashed"

cachedTmp :: (Binary a, Typeable a) => Prehashed Int a -> IO a
cachedTmp v = do
   tmpRoot <- getTemporaryDirectory
   cachedWithin (tmpRoot</>"hs-lazy-hashed") v

cachedWithin :: (Binary a, Typeable a)
                    => FilePath        -- ^ Storage directory
                    -> Prehashed Int a -- ^ Value to cache
                    -> IO a
cachedWithin path (Prehashed h v) = do
   let fname = path </> showHex (fromIntegral (h # typeRep [v]) :: Word64) [] <.> ".lhbs"
   cachedValueInFile fname v
