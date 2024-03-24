-- exercise 1
toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x <= 0 = []
    | x < 10 = [x]
    | otherwise = x `mod` 10 : toDigitsRev ( x `div` 10 )

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- exercise 2
doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft (x1:(x2:xs)) = x1 : 2 * x2 : doubleEveryOtherFromLeft xs
doubleEveryOtherFromLeft (x:[]) = x:[]
doubleEveryOtherFromLeft [] = []

doubleEveryOther = reverse . doubleEveryOtherFromLeft . reverse

-- exercise 3
sumDigits :: [Integer] -> Integer
sumDigits lst = sum $ concat $ map toDigits lst

-- exercise 4
sumDigitsInt :: Integer -> Integer
sumDigitsInt = sumDigits . doubleEveryOther . toDigits

validate :: Integer -> Bool
validate num = (sumDigitsInt num) `mod` 10 == 0

-- exercise 5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
    | n < 1 = []
    | n == 1 = [(a, b)]
    | otherwise = (hanoi (n - 1) a c b) ++ ((a, b) : (hanoi (n - 1) c b a))

