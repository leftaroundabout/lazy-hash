#!/bin/bash

rm -r ".hscache/lazy-hashed"

set -x

echo $'
{-# LANGUAGE QuasiQuotes #-}
import Data.LazyHash.Cache.Int
import Data.Numbers.Primes
import System.Environment

main :: IO ()
main = do
   [n] <- map read <$> getArgs
   nDecomp <- cached $ [fundamental|primeFactors|] <#> n
   print (nDecomp :: [Integer])
' > PrimeDecomposition.hs

time runhaskell PrimeDecomposition.hs 839876202089798609265694

time runhaskell PrimeDecomposition.hs 839876202089798609265694

time runhaskell PrimeDecomposition.hs 426409947997542378230229282

time runhaskell PrimeDecomposition.hs 839876202089798609265694
