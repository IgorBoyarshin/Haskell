for :: a -> (a -> Bool) -> (a -> a) -> (a -> IO ()) -> IO ()
for i p f job =
    if p i
        then do
            job i
            for (f i) p f job
        else
            return ()


divisors :: Int -> [Int]
divisors p = [d | d <- [1..p], mod p d == 0]

getDivisors :: [Int] -> [[Int]]
getDivisors xs = map divisors xs


applyToInts :: (Int -> Int) -> [Int] -> [Int]
applyToInts _ [] = []
applyToInts f (x:xs) = f x : applyToInts f xs

mulStuff :: Int -> [Int] -> [Int]
mulStuff _ [] = []
mulStuff a (x:xs) = (a*x) : mulStuff a xs

mulStuff2 :: Int -> [Int] -> [Int]
mulStuff2 a = applyToInts (a*)

-- ************************ --

takeInt :: Int -> [Int] -> [Int]
takeInt 0 _  = []
takeInt _ [] = []
takeInt n (mn:nn) = mn : takeInt (n-1) nn

dropInt :: Int -> [Int] -> [Int]
dropInt 0 a  = a
dropInt _ [] = []
dropInt n (x:xs) = dropInt (n-1) xs

sumInt :: [Int] -> Int
sumInt [] = 0
sumInt (x:xs) = x + sumInt xs

scanSum :: [Int] -> [Int]
scanSum [] = []
scanSum (a:[]) = a:[]
scanSum (a:b:as) = a : scanSum ((a+b):as)

diffs :: [Int] -> [Int]
diffs [] = []
diffs (a:[]) = []
diffs (a:b:as) = (a-b) : diffs (b:as)

difs :: [Int] -> [Int]
difs [] = []
difs (a:as) = difs2 (a:as) as
    where
        difs2 :: [Int] -> [Int] -> [Int]
        difs2 _ [] = []
        difs2 (a:as) (b:bs) = (b-a) : difs2 as bs
