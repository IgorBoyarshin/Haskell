import Data.List
import Data.Maybe
import Data.Ord
import System.IO

-- PRETTY SLOW

func 1 = 1
func n
    | even n    = div n 2
    | otherwise = 3 * n + 1

answer n = fst $ maximumBy (comparing snd) $ filter ((<n) . fst) $ foldl step [(1,1)] [2..n-1]
        where
            step acc x =
                if (lookup x acc /= Nothing) then acc
                            else let nacc = step acc next; next = func x in (x, 1 + (fromMaybe 0 $ lookup next nacc)):nacc

--              TASK                --
-- The following iterative sequence is defined for the set of positive integers:
--
-- n → n/2 (n is even)
-- n → 3n + 1 (n is odd)
--
-- Using the rule above and starting with 13, we generate the following sequence:
--
-- 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
-- It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms.
-- Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
--
-- Which starting number, under one million, produces the longest chain?
--
-- NOTE: Once the chain starts the terms are allowed to go above one million.
