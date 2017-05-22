-- |
-- Module      : Data.LazyHash.Cache
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.LazyHash.Cache where

import Data.LazyHash.Class
import Data.LazyHash.Numerical ()

import qualified Data.Hashable as SH

import Data.Binary
import System.FilePath
import System.Directory

import Data.Binary

import Data.Typeable

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BS (toStrict)

import qualified Data.ByteString.Base16 as B16

cached :: (Hash h, Binary a, Typeable a, Binary h) => Prehashed h a -> IO a
cached = cachedWithin ".hscache/lazy-hashed"

cachedTmp :: (Hash h, Binary a, Typeable a, Binary h) => Prehashed h a -> IO a
cachedTmp v = do
   tmpRoot <- getTemporaryDirectory
   cachedWithin (tmpRoot</>"hs-lazy-hashed") v

cachedWithin :: (Hash h, Binary a, Typeable a, Binary h)
                   => FilePath      -- ^ Storage directory
                   -> Prehashed h a -- ^ Value to cache
                   -> IO a
cachedWithin path (Prehashed h v) = do
   let fname = path </> (BS.unpack . B16.encode . BS.toStrict . encode
                           $ h # typeRep [v]) <.> ".lhbs"
   cachedValueInFile fname v

cachedValueInFile :: Binary a
      => FilePath      -- ^ File to store this value in.
      -> a             -- ^ Value to cache
      -> IO a
cachedValueInFile fname v
 = doesFileExist fname >>= \case
    True -> do
      vMemoized <- decodeFile fname
      return vMemoized
    False -> do 
      createDirectoryIfMissing True $ takeDirectory fname
      encodeFile fname v
      return v
