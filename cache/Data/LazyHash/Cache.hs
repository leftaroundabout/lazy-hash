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
{-# LANGUAGE TemplateHaskell  #-}

module Data.LazyHash.Cache (
             -- * The caching actions
               cached, cached'
             -- * Prehashing tools
             , fundamental, (<#>), liftPH2
             -- * Configuration
             , CacheAccessConf, cachingLocation
                              , usePrecalculated
                              , calculateIfNecessary
                              , burnAfterReading
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
import Control.Monad

import Data.Binary

import Data.Typeable

import qualified Data.ByteString.Char8 as BS hiding (hPut)
import qualified Data.ByteString.Lazy as BS (toStrict, hPut)

import qualified Data.ByteString.Base16 as B16

import Data.Default.Class

import Lens.Micro
import Lens.Micro.TH

data CacheAccessConf = CachAccConf {
      _cachingLocation :: Maybe FilePath
    , _usePrecalculated, _calculateIfNecessary
    , _writeUsedVersion, _burnAfterReading :: Bool
    }
makeLenses ''CacheAccessConf
instance Default CacheAccessConf where
  def = CachAccConf (Just ".hscache/lazy-hashed") True True True False

cached :: (Hash h, Binary a, Typeable a, Binary h) => Prehashed h a -> IO a
cached = cached' def

cached' :: (Hash h, Binary a, Typeable a, Binary h)
                   => CacheAccessConf
                   -> Prehashed h a -- ^ Value to cache
                   -> IO a
cached' conf@(CachAccConf Nothing _ _ _ _) v = do
   tmpRoot <- getTemporaryDirectory
   cached' (conf & cachingLocation .~ Just (tmpRoot</>"hs-lazy-hashed")) v
cached' conf@(CachAccConf (Just path) reuse calcNew writeUsed burnAfterReading)
          (Prehashed h v) = do
   let fname = path </> (BS.unpack . B16.encode . BS.toStrict . encode
                           $ h # typeRep [v]) <.> ".lhbs"
   cachedValueInFile conf fname v

cachedValueInFile :: Binary a
      => CacheAccessConf
      -> FilePath      -- ^ File to store this value in.
      -> a             -- ^ Value to cache
      -> IO a
cachedValueInFile (CachAccConf _ reuse calcNew writeUsed burn) fname v
 = doesFileExist fname >>= \case
    True | reuse -> do
      vMemoized <- decodeFile fname
      when burn $ removeFile fname
      return vMemoized
    _ | calcNew -> do 
      when writeUsed $ do
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
    False -> error "Requested value from cache that is not there. Perhaps enable `calculateIfNecessary`?"
