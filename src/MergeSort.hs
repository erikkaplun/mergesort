module MergeSort (mergesort) where

mergesort :: Ord a => [a] -> [a]
mergesort xs = mergesort' (<=) xs

mergesort' pred []   = []
mergesort' pred [x]  = [x]
mergesort' pred xs = merge pred (mergesort' pred xs1) (mergesort' pred xs2)
  where
    (xs1,xs2) = split xs

split :: [a] -> ([a],[a])
split xs = go xs xs where
  go (x:xs) (_:_:zs) = (x:us,vs) where (us,vs)=go xs zs
  go    xs   _       = ([],xs)

merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
merge pred xs []         = xs
merge pred [] ys         = ys
merge pred (x:xs) (y:ys)
  | pred x y = x: merge pred xs (y:ys)
  | otherwise = y: merge pred (x:xs) ys
