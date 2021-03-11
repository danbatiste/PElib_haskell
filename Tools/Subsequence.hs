module Tools.Subsequence where
import Data.List

--This could probably be done with 3 patterns.
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p []       = []
takeUntil p [x]      = [x]
takeUntil p [x,y]    = (x:m)
    where m    = if p x then [] else [y]
takeUntil p (x:y:zs) = (x:m) ++ rest
    where m    = if p x then [] else [y]
          rest = if p y then [] else takeUntil p zs

takeUntil p (a:[b])  = a:[b]
takeUntil p (a:b:cs) = a:b:takeWhile (/=b) cs

consecr :: (Num a, Eq a) => a -> [a] -> Bool
consecr n xs
  | null xs       = False
  | n == (sum xs) = True
  | otherwise     = consecr n (init xs)
  
consecl :: (Num a, Eq a, Ord a) => a -> [a] -> Bool
consecl x xs = elem x $ takeWhile (<=x) [ sum (take n xs) | n <- [1..length xs]]

consecPermSums :: (Num a, Eq a) => [a] -> [a]
consecPermSums xs = (nub . fmap product . filter (`isSubsequenceOf` xs) . subsequences) xs

sumConsec :: Integral a => [a] -> [a]
sumConsec xs = [ sum (take n xs) | n <- [0..length xs]]

rotate :: (Num t, Eq t) => t -> [a] -> [a]
rotate _ []     = []
rotate _ [x]    = [x]
rotate 0 xs     = xs
rotate n xs     = rotate (n-1) $ concat [[last xs], init xs]

rotations :: [a] -> [[a]]
rotations xs = [rotate n xs | n <- [0..l]]
    where l = length xs - 1

shiftWith :: Int -> [a] -> [[a]]
shiftWith n xs = takeWhile ((==n) . length) [ take n (drop m xs) | m <- [0..] ]
--DEPRECATED:: shiftWith n xs = [ take n (drop m xs) | m <- [0..length xs - n] ]

equidistant :: (Num a, Eq a) => [a] -> Bool
equidistant []       = error "Must be at least 2 elements in list"
equidistant [x]      = error "Must be at least 2 elements in list"
equidistant xs@(_:_) = (1==) . length . nub . fmap diff . shiftWith 2 $ xs
    where diff (n:[m]) = m - n