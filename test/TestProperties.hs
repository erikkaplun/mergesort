module TestProperties where

import Debug.Trace
import qualified MergeSort as PS

isSorted :: Ord a => [a] -> Bool
isSorted (x:xs@(y:ys)) = x <= y && isSorted xs
isSorted _ = True

propMergesort :: [Int] -> Bool
propMergesort xs = isSorted $ PS.mergesort (traceShow "xs: " xs)
