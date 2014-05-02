-- Problem 1
toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0    = []
    | n < 10    = [n]
    | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0    = []
    | n < 10    = [n]
    | otherwise = [n `mod` 10] ++ toDigits (n `div` 10)

-- Problem 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []     = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:zs)
    | length zs `mod` 2 == 0 = x*2 : y : doubleEveryOther zs
    | otherwise              = x : y*2 : doubleEveryOther zs

-- Problem 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (sum (toDigits x)) + sumDigits xs

-- Problem 4
validate :: Integer -> Bool
validate n = (sumDigits (doubleEveryOther(toDigits n))) `mod` 10 == 0
-- validate = (==) 0 . flip mod 10 . sumDigits . doubleEveryOther . toDigits

-- Problem 5
