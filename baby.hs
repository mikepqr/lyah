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
