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
                              , writeUsedVersion
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
makeLensesWith (lensRules & generateSignatures.~False) ''CacheAccessConf

-- | Where the cache-files should be stored. If 'Nothing', the system temporary
--   folder will be used.
cachingLocation :: Lens' CacheAccessConf (Maybe FilePath)
-- | Whether to actually make use of a cached value, in case one is found. Usually,
--   doing that is the entire point of this library, but sometimes you may want
--   to disable it (e.g. after debbuging some function that was assumed 'fundamental').
usePrecalculated :: Lens' CacheAccessConf Bool
-- | Whether you want the processor to bite the bullet and compute the value itself,
--   in case it /can't/ be found in the cache. Again, you will need to have this on
--   at some point (the cached values have to come from somewhere, after all).
calculateIfNecessary :: Lens' CacheAccessConf Bool
-- | Whether to store the computed value in cache. This too should usually be enabled.
writeUsedVersion :: Lens' CacheAccessConf Bool
-- | Enable this to have the cached value deleted after use. May be useful to save
--   disk space.
--
--   (For the record: this does not perform any kind of special secure-memore-erasing,
--   it only removes the cache file.)
burnAfterReading :: Lens' CacheAccessConf Bool

instance Default CacheAccessConf where
  def = CachAccConf (Just ".hscache/lazy-hashed") True True True False

-- | Look up a value in the project-global cache store. If it has already been
--   computed during an earlier program run, simply re-use that result, else
--   calculate it and store for future runs to use.
--
--   This is a shortcut for @'cached'' 'def'@, which corresponds to the options
--
--   * @'cachingLocation' .~ (Just ".hscache/lazy-hashed")@
--   * @'usePrecalculated' .~ True@
--   * @'calculateIfNecessary' .~ True@
--   * @'writeUsedVersion' .~ True@
--   * @'burnAfterReading' .~ False@
--
--   This function is polymorphic in the type of hash it uses, but this can require
--   boilerplate signature and you'll probably want to choose one such type and stick
--   to it for your entire project. We offer specialised versions for this purpose;
--   see "Data.LazyHash.Cache.Int".
cached :: (Hash h, Binary a, Typeable a, Binary h) => Prehashed h a -> IO a
cached = cached' def

-- | Write, re-use or modify the cache, depending on the configuration.
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
