# PElib_haskell
Haskell libraries for solving Project Euler problems


# Documentation
```
Tools.Factor
    hasFactor           :: Integral a => a -> a -> Bool
    hasfactor n m       -> Checks whether `m` is a factor of `n`.
    
    hasFactors          :: Integral a => a -> [a] -> Boo
    hasFactors n xs     -> Checks whether all elements of `xs` are factors of `n`.
    
    ulamCorners         :: (Num a, Enum a) => a -> [a]
    ulamCorners n       -> Finds corners of the ulam spiral that are a distance `n` 
                           away from the origin.
    
    factors             :: Integral a => a -> [a]
    factors n           -> Creates a list containing the prime factorization of `n`.
                           EX: `factors 24`                     --> `[2,2,2,3]`

    divisors            :: Integer a => a -> [a]
    divisors n          -> Creates a list of all numbers by which `n` is divisible.
                           EX `divisors 24`                     --> `[1,2,4,8,3,6,12,24]`

    isPrime             :: Integral a => a -> Bool
    isPrime n           -> Determines primality of `n`.
    
    primesFromTo        :: Integral a => a -> a -> [a]
    primesFromTo n m    -> Enumerates primes from `n` to `m`.
    
    primesFrom          :: Integral a => a -> [a]
    primesFrom n        -> Enumerates primes from `n` onward.
    
    primesTo            :: Integral a => a -> [a]
    primesTo            -> Enumerates primes to `n`.
    
    
    
Tools.Subsequence
    takeUntil           :: (a -> Bool) -> [a] -> [a]
    takeUntil p xs      -> Returns a list taken until the predicate `p` evaluates to `True`.
                           EX: `takeUntil (==3) [1..10]`        --> `[1,2,3,4,5,6,7]`

    consecr             :: (Num a, Eq a) => a -> [a] -> Bool
    consecr n xs        -> Checks whether `n` is the sum of a consecutive series of elements in
                           `xs`. Begins checking for the sum of the entire list, and then works
                           down to the empty list `[]`. `consecr` is right-associative.
                           EX: `consecl (1+2+3+4+5+6) [1..6]`   --> `True`
                               `consecl 21 [1..5]`              --> `False`
                           
    consecl             :: (Num a, Eq a) => a -> [a] -> Bool
    consecl n xs        -> Checks whether `n` is the sum of a consecutive series of elements in
                           `xs`. Begins with a list containing the first element of xs and works 
                           up to the full list. `consecl` is left-associative.
                           EX: `consecl 21 [1..]`               --> `True`
                               `consecl (1+2+3+4+5+6) [1..6]`   --> `True`
                               `consecl 21 [1..5]`              --> `False`
                           
    consecPermSums      :: (Num a, Eq a) => [a] -> [a]
    consecPermSums xs   -> Finds the sum of all subsequences of `xs`.
                           EX: `consecPermSums [1,2,3,4]`       --> `[1,2,3,6,4,8,12,24]`
                               `consecPermSums [1,2,3,4]`       --> `[1,2*1,3*1,3*2,4*1,4*2,4*3,4*3*2]`
    
    sumConsec           :: Integral a => [a] -> [a]
    sumConsec xs        -> Returns a list of the elements in `xs` consecutively summed.
                           EX: `sumConsec [1..4]`               --> `[1, 1+2, 1+2+3, 1+2+3+4]`
     
    rotate              :: (Num t, Eq t) => t -> [a] -> [a]
    rotate n xs         -> Rotates a list `xs` `n` places forward.
                           EX: `rotate 2 [1..7]`             --> `[6,7,1,2,3,4,5]`

    rotations           :: [a] -> [[a]]
    rotations xs        -> Creates a list containing all possible rotations of `xs`.
                           EX: `rotations [1..4]`               --> `[[1,2,3,4],[4,1,2,3],[3,4,1,2],[2,3,4,1]]`
                           
    shiftWith           :: Int -> [a] -> [[a]]
    shiftWith n xs      -> Puts elements `0` through `n` in a list, then `1` through `n+1`, and 
                           so on until the end of the list is reached.
                           EX: `shiftWith 3 [1..5]`             --> `[[1,2,3],[2,3,4],[3,4,5]]`
                           
    equidistant         :: (Num a, Eq a) => [a] -> Bool
    equidistant xs      -> Checks whether all elements in a list are equally distant from one
                           another.
                           EX: `equidistant [1,30,60,90]`       --> `False`
                               `equidistant [0,20,40,60]`       --> `True`
                               


Tools.Number
    digit               :: (Integral a, Show a) => Int -> a -> Integer
    digit n m           -> Returns the `n`th digit of `m`. 

    toDigits            :: (Integral a, Show a) => a -> [Integer]
    toDigits n          -> Enumerates the digits of `n`.
    
    fromDigits          :: (Integral a, Show a) => [a] -> Integer
    fromDigits ns       -> Concatenates a list of digits into a single number.
                           EX: `fromDigits [1,2,3]`             --> `123`
    
    factorial           :: Integral a => a -> a
    factorial n         -> Evaluates to the factorial of `n`.
    
    toWords             :: Integer -> [Char]
    toWords n           -> Converts an integer `n` to its string form for all `n < 10000`
    
    triangle            :: Integral a => a -> a
    triangle n          -> Evaluates to the `n`th triangle number.
    
    invTriangle         :: Integral a => a -> a
    invTriangle n       -> The inverse of `triangle`. Finds a triangle number's position in the
                           triangular series.
                           EX: `invTriangle (triangle 7)`       --> `7`
                               `invTriangle 55`                 --> `10`
    
    isTriangle          :: Integral a => a -> Bool
    isTriangle          -> Checks whether `n` is a triangle number.
    
    pent                :: Integral a => a -> a
    pent n              -> Evaluates to the `n`th pentagonal number.
    
    invPent             :: Integral a => a -> a
    invPent n           -> The inverse of `pent`. Finds a pentagonal number's position in the
                           pentagonal series.
                           EX: `invPent (pent 5)`               --> `5`
                               `invPent 35`                     --> `5`
    
    isPent              :: Integral a => a -> Bool
    isPent n            -> Checks whether `n` is a pentagonal number
 
 
 
Tools.Logic
    both                :: (a -> Bool) -> (a -> Bool) -> a -> Bool
    both p1 p2 val      -> Checks whether `val` satisfies predicates `p1` and `p2`.                 ## Isn't this just `&&`?
```
