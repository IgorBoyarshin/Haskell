import Data.List
import System.IO

answer = head $ foldr addIfDiv [] (reverse [2..(ceiling . sqrt $ fromIntegral x)])
            where addIfDiv a b
                    | x `mod` a == 0 = addIfPrime a b
                    | otherwise = b
                  addIfPrime m n
                    | all (/=0) $ map (mod m) n = m:n
                    | otherwise = n

--          TASK             --
-- The prime factors of 13195 are 5, 7, 13 and 29.
--
-- What is the largest prime factor of the number 600851475143 ?
