import Data.List
import System.IO

isPalindrome x = (show x) == (reverse $ show x)
palindromes lBound uBound = [(a,b) | a <- [lBound..uBound], b <- [lBound..uBound], isPalindrome $ a * b]
answer = maximum $ map (\(a,b) -> a*b) $ palindromes 800 999

--          TASK             --
-- A palindromic number reads the same both ways. The largest palindrome made from
-- the product of two 2-digit numbers is 9009 = 91 × 99.
--
-- Find the largest palindrome made from the product of two 3-digit numbers.
