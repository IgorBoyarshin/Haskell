import Data.List
import System.IO
import Control.Applicative
import Control.Monad
import Data.List
import Data.Char
import System.IO

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
 
sieveUA :: Int -> UArray Int Bool
sieveUA top = runSTUArray $ do
    let m = (top-1) `div` 2
        r = floor . sqrt $ fromIntegral top + 1
    sieve <- newArray (1,m) True      -- :: ST s (STUArray s Int Bool)
    forM_ [1..r `div` 2] $ \i -> do
      isPrime <- readArray sieve i
      when isPrime $ do               -- ((2*i+1)^2-1)`div`2 == 2*i*(i+1)
        forM_ [2*i*(i+1), 2*i*(i+2)+1..m] $ \j -> do
          writeArray sieve j False
    return sieve
 
primesToUA :: Int -> [Int]
primesToUA top = 2 : [i*2+1 | (i,True) <- assocs $ sieveUA top]

isPrime :: Int -> Bool
isPrime x = (x `elem`) $ primesToUA x


-- ghci
-- :l main.hs
-- :r
-- :i info about Base class
