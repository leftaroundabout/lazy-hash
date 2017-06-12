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

module Data.LazyHash.Cache (
             -- * The caching actions
               cached, cachedTmp, cachedWithin
             -- * Prehashing tools
             , fundamental, (<#>), liftPH2
             -- * Internals
             , cachedValueInFile
                               ) where

import Data.LazyHash.Class
import Data.LazyHash.Numerical ()

import qualified Data.Hashable as SH

import Data.Binary
import System.FilePath
import System.Directory
import System.IO
import System.IO.Temp
import Control.Exception (bracket)

import Data.Binary

import Data.Typeable

import qualified Data.ByteString.Char8 as BS hiding (hPut)
import qualified Data.ByteString.Lazy as BS (toStrict, hPut)

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
      let storageDir = takeDirectory fname
          wipDir = storageDir</>"wip"
      createDirectoryIfMissing True storageDir
      createDirectoryIfMissing True wipDir
      bracket
        ( openBinaryTempFile wipDir (takeFileName fname++".") )
        ( \(_, h) -> hClose h )
        ( \(tmpFname, h) -> do
            BS.hPut h $ encode v
            renameFile tmpFname fname
        )
      return v
