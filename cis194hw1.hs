-- Validating Credit Card Numbers

import qualified Data.Char

lastDigit :: Integer -> Integer
lastDigit n = toInteger . (Data.Char.digitToInt) . last $ show n

dropLastDigit :: Integer -> Integer
dropLastDigit n = read ((init . show) n) :: Integer

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = map (toInteger . (Data.Char.digitToInt)) $ show n

doubleOther :: [Integer] -> [Integer]
doubleOther [x] = [x]
doubleOther [x,y] = [x, 2*y]
doubleOther (x:y:xs) = x : y * 2 : doubleOther xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse . doubleOther $ reverse xs

sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ map (sum . toDigits) xs

validate :: Integer -> Bool
validate n = (sumDigits . doubleEveryOther . toDigits) n `mod` 10 == 0

-- Hanoi
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n == 1 = [(a,b)]
  | otherwise = (hanoi (n-1) a c b) ++ [(a,b)] ++ (hanoi (n-1) c b a)
