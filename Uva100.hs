-- The 3n + 1 problem

import IOHelpers
import Data.List (foldl')


next :: Integer -> Integer
next 1 = 1
next n | n `mod` 2 == 1 = 3 * n + 1
       | otherwise      = n `div` 2

getSeq 1 = [1]
getSeq n = n : getSeq (next n)

cycleLength = length . getSeq

maximum' (x:xs) = foldl' max x xs

maxCycleLength i j = maximum' $ map cycleLength [i..j]


processLine ln = let ns = map read (words ln) :: [Integer]
                     (i:j:_) = ns
                     m = maxCycleLength i j
                 in unwords $ map show [i, j, fromIntegral m]

main = printTime $ processFile "Input100.txt" "Output100.txt" processLine
-- Interpreted:
-- Time: 0.093750 sec.
