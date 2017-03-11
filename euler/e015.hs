import Data.List
import System.IO

-- DIRECT
f 0 _ = 0
f _ 0 = 0
f 1 1 = 1
-- f n m = f (n-1) m + f n (m-1)
-- f n n = 2 * (f (n-1) n)
f n m
    | n == m    = 2 * (f (n-1) m)
    | n > m     = f m n
    | otherwise = f (n-1) m + f n (m-1)

answer x = f (x+1) (x+1)

-- BETTER
answer2 = div (product [21..40]) $ product [1..20]

--              TASK                --
-- Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down,
-- there are exactly 6 routes to the bottom right corner.
-- How many such routes are there through a 20×20 grid?
