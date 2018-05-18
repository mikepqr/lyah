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
