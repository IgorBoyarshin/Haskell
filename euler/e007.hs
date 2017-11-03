import Data.List
import System.IO

primes = 2 : filter (null . tail . primeFactors) [3,5..]

primeFactors n = factor n primes
  where
    factor n (p:ps)
        | p*p > n        = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise      =     factor n ps

nthPrime n = primes !! (n-1)

 -- Works, but slow
getNextPrime primesBefore = head $ dropWhile (not . isPrime) [last primesBefore + 1..]
            -- where isPrime x = foldr (\n res -> res && (mod x n /= 0)) True primesBefore
            where isPrime x = all (/=0) $ map (mod x) primesBefore

step 0 primes = primes
step n [] = step (n - 1) [2]
step n primes = step (n - 1) $ primes ++ [getNextPrime primes]
answer n = last $ step n []

--          TASK             --
-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
--
-- What is the 10 001st prime number?
