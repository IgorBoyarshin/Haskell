import Data.List
import Data.Char
import System.IO

answer :: Int -> Int
answer = sum . map digitToInt . show . (2^)

--              TASK                --
-- 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
--
-- What is the sum of the digits of the number 2^1000?
