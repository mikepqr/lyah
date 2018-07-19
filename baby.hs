doubleMe x = x + x

doubleUs x y = x * 2 + y * 2

doubleSmallNumber x = if x > 100
                        then x
                        else x * 2

doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

fizzBuzz xs = [if x `mod` 5 == 0 && x `mod` 3 == 0 then "FizzBuzz"
                else if x `mod` 3 == 0 then "Fizz"
                else if x `mod` 5 == 0 then "Buzz"
                else show x | x <- xs]

f :: Int -> String
f 1 = "One"
f x = show x

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

fib :: Int -> Int
fib 1 = 0
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)

bmiTell :: Double -> String
bmiTell bmi
    | bmi < 18.5 = "underweight"
    | bmi < 25 = "good"
    | bmi < 30 = "overweight"
    | otherwise = "obese"


max' :: Ord a => a -> a -> a
max' x y
    | x > y         = x
    | otherwise     = y

bmiCalcTell :: Double -> Double -> String
bmiCalcTell weight height
    | bmi < skinny = "underweight"
    | bmi < good = "good"
    | bmi < overweight = "overweight"
    | otherwise = "obese"
    where bmi = weight / height^2
          (skinny, good, overweight) = (18.5, 25, 30)
cylinder :: Double -> Double -> Double
cylinder r h = 
    let sideArea = 2 * pi * r * h
        topArea = pi * r^2
    in  sideArea + 2 * topArea

squares :: [Double] -> [Double]
squares xs = [square | x <- xs, let square = x * x]

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x : replicate' (n-1) x

take' :: Int -> [a] -> [a]
take' n xs
    | n <= 0 = []
    | n == 1 = [head xs]
    | otherwise = head xs : take' (n-1) (tail xs)

take'' :: Int -> [a] -> [a]
take'' n xs
    | n <= 0 = []
take'' _ [] = []
take'' n (x:xs) = x : take'' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = a `elem'` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerOrEqual = [a | a <- xs, a <= x]
        larger = [a | a <- xs, a > x]
    in  quicksort smallerOrEqual ++ [x] ++ quicksort larger

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

contains10 :: [Int] -> Bool
contains10 = (10 `elem`)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
    | p x = x : takeWhile' p xs
    | otherwise = []

addThree :: Int -> Int -> Int -> Int
addThree a b c = a + b + c

addThree' :: Int -> Int -> Int -> Int
addThree' = \a -> (\b -> (\c -> a + b + c))

sum' xs = foldl f 0 xs
    where f a b = a + b

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
oddSquareSum' = (sum . takeWhile (<10000) . filter odd . map (^2))
