import Data.List
import System.IO

-- SLOW
divisorsOfTriangles = map divisors triangles
triangles           = map fst $ iterate (\(a,b) -> (a+b+1, b+1)) (1,1)
divisors n = foldr (\x acc -> if (mod n x == 0) then x:(div n x):acc else acc) [] $ takeWhile (\x -> x < div n x) [1..n]
answer n = product $ take 2 $ head $ dropWhile (\x -> length x < n) divisorsOfTriangles

-- MUCH BETTER
triangles2          = scanl1 (+) [1..]
answer2 = head $ filter ((>500) . nDivisors) triangles2
            where nDivisors n = product $ map ((+1) . length) (group (primeFactors n))
                  primes = 2 : filter (null . tail . primeFactors) [3,5..]
                  primeFactors n = factor n primes
                                    where factor n (p:ps)
                                            | p*p > n        = [n]
                                            | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
                                            | otherwise      =     factor n ps
-- answer n = head $ dropWhile (\x -> length x < n) divisorsOfTriangles

--              TASK                --
-- The sequence of triangle numbers is generated by adding the natural numbers. So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first ten terms would be:
--
-- 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
--
-- Let us list the factors of the first seven triangle numbers:
--
--  1: 1
--  3: 1,3
--  6: 1,2,3,6
-- 10: 1,2,5,10
-- 15: 1,3,5,15
-- 21: 1,3,7,21
-- 28: 1,2,4,7,14,28
-- We can see that 28 is the first triangle number to have over five divisors.
--
-- What is the value of the first triangle number to have over five hundred divisors?
