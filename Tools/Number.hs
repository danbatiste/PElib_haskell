module Tools.Number where

digit :: (Integral a, Show a) => Int -> a -> Integer
digit n = (!! n) . reverse . toDigits

toDigits :: (Num a, Show a) => a -> [Integer]
toDigits n = [ rd [x] | x <- xs ]
    where rd = read :: [Char] -> Integer
          xs = filter (/='.') . show $ n
          
fromDigits :: (Integral a, Show a) => [a] -> Integer
fromDigits [] = 0
fromDigits ns = (read::[Char]->Integer) . concat . fmap show $ ns
          
factorial :: Integral a => a -> a
factorial = product . enumFromTo 1

toWords :: Integer -> [Char]
toWords n
  | n < 1     = ""
  | n < 20    = case n of
                  1  -> "one"
                  2  -> "two"
                  3  -> "three"
                  4  -> "four"
                  5  -> "five"
                  6  -> "six"
                  7  -> "seven"
                  8  -> "eight"
                  9  -> "nine"
                  10 -> "ten"
                  11 -> "eleven"
                  12 -> "twelve"
                  13 -> "thirteen"
                  15 -> "fifteen"
                  18 -> "eighteen"
                  _ -> (toWords $ digit 0 n) ++ "teen"
  | n < 100   = do
                  let r = ' ':(toWords $ (digit 0 n))
                  case digit 1 n of
                      2  -> "twenty" ++ r
                      3  -> "thirty" ++ r
                      4  -> "forty" ++ r
                      5  -> "fifty" ++ r
                      6  -> "sixty" ++ r
                      7  -> "seventy" ++ r
                      8  -> "eighty" ++ r
                      9  -> "ninety" ++ r
  | n < 1000  = do
                  let r = (toWords $ digit 2 n) ++ " hundred"
                  case mod n 100 of
                      0  -> r
                      _  -> r ++ " and " ++ (toWords $ mod n 100)
  | n < 10000 = (toWords $ digit 3 n) ++ " thousand " ++ (toWords $ mod n 1000)

triangle :: Integral a => a -> a
triangle = (`div` 2) . ((*) <*> (+1))

invTriangle     :: Integral a => a -> a
invTriangle n = floor . (/ 2) . sqrt . fromIntegral $ 8 * n + 1

isTriangle :: Integral a => a -> Bool
isTriangle n = n == (triangle . invTriangle $ n)

pent :: Integral a => a -> a
pent n = div (n * (3*n - 1)) 2

invPent :: Integral a => a -> a
invPent n = ceiling . (/ 6) . sqrt . fromIntegral $ 24 * n + 1

isPent :: Integral a => a -> Bool
isPent n = n == (pent . invPent $ n)

hex :: Integral a => a -> a
hex n = n * (2*n - 1)

invHex :: Integral a => a -> a
invHex n = ceiling . (/ 4) . sqrt . fromIntegral $ 8 * n + 1

isHex :: Integral a => a -> Bool
isHex n = n == (hex . invHex $ n)