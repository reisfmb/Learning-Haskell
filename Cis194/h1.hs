import Data.List
import Data.Maybe

-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits x
    | x <= 0    = []
    | otherwise = [ toInteger (fromEnum  y - fromEnum '0') | y <- show x ]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

-- Exercise 2
calculateMultiplier :: Int -> Integer
calculateMultiplier x = if x `mod` 2 == 1 then 2 else 1

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = 
    let k = reverse l
    in reverse [ m*y |  y <- k, 
                        let index   = fromJust (elemIndex y k),
                        let m       = calculateMultiplier index ]

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits l = head l + sumDigits (tail l)

-- Exercise 4
validate :: Integer -> Bool
validate x = if mod x 8 == 0 then True else False

-- Exercise 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n from to aux
    | n == 1 = [(from, to)]
    | otherwise = hanoi (n-1) from aux to ++ [(from, to)] ++ hanoi (n-1) aux to from