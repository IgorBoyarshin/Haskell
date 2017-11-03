import Data.List
import System.IO

anotherSolution = foldr1 lcm [1..20]

nsk number = product $ foldr addIfNeeded [] $ reverse [2..number]
        where addIfNeeded x [] = [x]
              addIfNeeded x (l:ls)
                        | x == 1 = l:ls
                        | x `mod` l == 0 = l:(addIfNeeded (x `div` l) ls)
                        | otherwise = l:(addIfNeeded x ls)

--          TASK             --
-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
--
-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
