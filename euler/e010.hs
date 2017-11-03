import Data.List
import System.IO

primes = 2 : filter (null . tail . primeFactors) [3,5..]
primeFactors n = factor n primes
                    where factor n (p:ps)
                            | p*p > n        = [n]
                            | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
                            | otherwise      =     factor n ps

answer n = sum $ takeWhile (<n) primes

--              TASK                --
-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
--
-- Find the sum of all the primes below two million.
