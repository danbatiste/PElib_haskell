module Tools.Factor where
import Data.List

hasFactor :: Integral a => a -> a -> Bool
hasFactor n m    = mod n m == 0

hasFactors :: Integral a => a -> [a] -> Bool
hasFactors n xs  = and [n `hasFactor` x | x <- xs]

ulamCorners :: (Num a, Enum a) => a -> [a]
ulamCorners n = [base + c*oddN | c <- [0..3], let oddN = (2*n) - 1, let base = oddN^2 + oddN + 1 + c]

factors :: Integral a => a -> [a]
factors 1 = []
factors n = k : (factors $ div n k)
    where k = head [x | x <- [2..n], mod n x == 0]
          upperBound = floor . sqrt . fromIntegral $ n

divisors :: Integral a => a -> [a]          
divisors = nub . fmap product . nub . subsequences . factors

isPrime :: Integral a => a -> Bool
isPrime 2 = True
isPrime n = 
    let upperBound = floor . sqrt . fromIntegral $ n
        in case [x | x <- 2:[3,5..upperBound], mod n x == 0] of
            [] -> True
            (x:xs) -> False
          
primesFromTo :: Integral a => a -> a -> [a]
primesFromTo n m  
  | even n    = filter isPrime (n:[n+1,n+3..m])
  | otherwise = filter isPrime [n,n+2..m]
    
primesFrom :: Integral a => a -> [a]
primesFrom n
  | even n    = filter isPrime (n:[n+1,n+3..])
  | otherwise = filter isPrime [n,n+2..]
  
primesTo :: Integral a => a -> [a]
primesTo n = 2:primesFromTo 3 n