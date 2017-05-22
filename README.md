A simple library that allows you to “pre-emptively” obtain hashes
of values that aren't even calculated (i.e. brought to normal form) yet.
The idea is to hash the _implementation_ rather than the result, i.e.
basically the source code (in AST form).

While this loses some properties of value-hashes (in particular, equal
values won't necessarily have the same hash), it has considerable
performance advantages.

The main application (see `cache` subpackage) is to use the hashes as
keys for a database of cached values (stored in binary form in files).
This allows “backing up” the results of expensive computations, so upon
re-running your program, it will not have to spend the same long
computation time again. Whereas any change to the parameters or the
behaviour of the source code will change the hash and thereby trigger
a recomputation (which is inevitable in this case because the result is
actually different).

```bash
$ echo '
> {-# LANGUAGE QuasiQuotes #-}
> import Data.LazyHash.Cache.Int
> import Data.Numbers.Primes
> import System.Environment
> 
> main :: IO ()
> main = do
>    [n] <- map read <$> getArgs
>    nDecomp <- cached $ [fundamental|primeFactors|] <#> n
>    print (nDecomp :: [Integer])
> ' > PrimeDecomposition.hs

$ runhaskell PrimeDecomposition.hs 839876202089798609265694      # A
[2,11083,17203,17192521,128110343]

real	0m5.485s
user	0m5.379s
sys	0m0.117s

$ runhaskell PrimeDecomposition.hs 839876202089798609265694      # B
[2,11083,17203,17192521,128110343]

real	0m0.569s
user	0m0.475s
sys	0m0.098s

$ runhaskell PrimeDecomposition.hs 426409947997542378230229282   # C
[2,3,19,37,71,6619,18805867,11438699803]

real	0m6.318s
user	0m6.250s
sys	0m0.080s

$ runhaskell PrimeDecomposition.hs 839876202089798609265694      # D
[2,11083,17203,17192521,128110343]

real	0m0.578s
user	0m0.498s
sys	0m0.082s
```

Notice how `B` and `D` finish almost instantaneously: they don't actually
bother _computing_ the prime decomposition, instead they just look it up
from a cache file, left over from `A`.
