#!/bin/bash

rm -r ".hscache/lazy-hashed"

set -x

echo $'
{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
import Data.LazyHash.Class
import Data.LazyHash.Cache.Int
import Data.Numbers.Primes
import System.Environment

main :: IO ()
main = do
   [n :: Prehashed Int Integer] <- map read <$> getArgs
   nDecomp <- cached $ liftPH [fundamental|primeFactors|] n
   print nDecomp
' > PrimeDecomposition.hs

time runhaskell PrimeDecomposition.hs 839876202089798609265694

time runhaskell PrimeDecomposition.hs 839876202089798609265694

time runhaskell PrimeDecomposition.hs 426409947997542378230229282

time runhaskell PrimeDecomposition.hs 839876202089798609265694
