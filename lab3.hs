import Data.List
import Data.Char
import System.IO

theText = "Hello there! + How do you do? 'Do' you say hello there where you are from?"

-- Named solution
theWords = map (map toLower) . map (takeWhile isLetter) . words $ theText
theWordsUnique = foldr (\el acc -> if (el `elem` acc) then acc else (el:acc)) [] theWords
sortedWords = sort theWordsUnique

-- The shortest solution
answer3 = sort . nub . map (map toLower) . map (takeWhile isLetter) . words $ theText

-- The most correct solution
answer4 = sort . delete "" . nub . map (map toLower) . map (foldr (\el acc -> if (isLetter el) then (el:acc) else acc) []) . words $ theText

-- Solution without the nub func
answer = sort . foldr addIfNotDuplicate [] . map (map toLower) . map (takeWhile isLetter) . words $ theText
        where addIfNotDuplicate el acc
                        | el `elem` acc = acc
                        | otherwise = el:acc

-- Prev solution written in one line
answer2 = sort . foldr (\el acc -> if (el `elem` acc) then acc else (el:acc)) [] . map (map toLower) . map (takeWhile isLetter) . words $ theText


-- Func definitions


map' _ []     = []
map' f (x:xs) = f x : map' f xs

takeWhile' _ []          =  []
takeWhile' p (x:xs)
            | p x       =  x : takeWhile' p xs
            | otherwise =  []

filter' _pred []          = []
filter' pred (x:xs)
              | pred x    = x : filter' pred xs
              | otherwise = filter' pred xs

quickSort [] = []
quickSort (x:xs) = quickSort (filter (<=x) xs) ++ [x] ++ quickSort (filter (>x) xs)
