## Chapter 3: Function syntax

_Pattern matching_ in its simplest form is kind of like a case statement that
matches on the _structure_ or type of the input, rather than an arbitrary
condition. E.g. given this definition

    f :: Int -> String
    f 1 = "One"
    f 2 = "Two"
    f x = "other"

    > f 1
    "One"
    > f 2
    "Two"
    > f 5
    "Other"

Note `f 1` and `f 2` work if there is no `f x` pattern in the function
definition, but in that case `f 3` results in a "Non-exhaustive patterns"
exception.

Each pattern is tested until a match is found (and then no more are tested, so
the first matching pattern is used).

Patterns allow recursive function definitions with base case, e.g.

    factorial :: Int -> Int
    factorial 0 = 1
    factorial n = n * factorial (n - 1)

Pattern matching tuples

    addvectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
    addvectors (x1, y1), (x2, y2) = (x1 + x2, y1 + y2)

As an example, recall `fst` and `snd` only work with tuples of length 2. We can
write a `middle` function for triples like this:

    middle :: (a, b, c) -> b
    (_, x, _) = x

Pattern matching lists in comprehensions (very similar to python):

    > let xs [(1, 2), (3, 4)]
    > [a + b | (a, b) <- xs]
    [3, 7]

If an element of the list does not match the pattern, it is skipped.

You can also pattern match lists with `:` and `[]`, `x:xs` binds the head to
`x` and the rest of the list to `xs`, so we can reimplement `head` like this:

```
head' :: [a] -> a
head' [] = error "Can't call head on empty list"
head' (x:_) = x
```

The parentheses are required for parsing. This function returns a string
containing no more than the first two elements of a list:

```
tell :: (Show a) => [a] -> String
tell [] = "empty list"
tell (x:[]) = "one element: " ++ show x
tell (x:y:[]) = "two elements: " ++ show x ++ " " ++ show y
tell (x:y:_) = "long list, first two are: " ++ show x ++ " " ++ show y
```

This line

    tell (x:[]) = "one element: " ++ show x

can be rewritten as 

    tell [x] = "one element: " ++ show x

which seems nicer to me.

_As-patterns_ allow you to break a list up while still keeping a reference to
the original item. They are preceded with `@`. E.g.

```
firstletter :: String -> String
firstletter "" = error "empty string"
firstletter all@(x:xs) = "the first letter of " ++ all ++ " is " ++ [x]
```

_Guards_ seem a little more general than patterns. They're like if-then-else
compound statements. E.g.

    bmiTell :: Double -> String
    bmiTell bmi
        | bmi < 18.5 = "underweight"
        | bmi < 25 = "good"
        | bmi < 30 = "overweight"
        | otherwise = "obese"

Note the pipe character and `otherwise` keyword. You can combine patterns and
guards, although there's no example in the book.

Guards work with functions with more than one paramter, e.g. here's a
reimplementation of `max`

    max' :: Ord a => a -> a -> a
    max' x y
        | x > y         = x
        | otherwise     = y

Reminder about the function type. It says: "max takes two items of type `a`
(where `a` is a member of the type class `Ord`, i.e. items that can be ordered,
i.e. items that work with `>` etc.) and returns a single item of type `a`"

You can use `where` to calculate variables that are local to a pattern (or if a
function has only one pattern, local to the function), e.g.

    bmiCalcTell :: Double -> Double -> String
    bmiCalcTell weight height
        | bmi < 18.5 = "underweight"
        | bmi < 25 = "good"
        | bmi < 30 = "overweight"
        | otherwise = "obese"
        where bmi = weight / height^2

or

    bmiCalcTell :: Double -> Double -> String
    bmiCalcTell weight height
        | bmi < skinny = "underweight"
        | bmi < good = "good"
        | bmi < overweight = "overweight"
        | otherwise = "obese"
        where bmi = weight / height^2
              (skinny, good, overweight) = (18.5, 25, 30)

Note the second line of `where` must be indented to match the first.

The `where` block can also contain function definitions.

`let-in` is like `where`, but more local. `where` is local to the particular
pattern or function, while `let` definitions are local to an expression. E.g.
to calculate a cylinder's surface:

    cylinder :: Double -> Double -> Double
    cylinder r h = 
        let sideArea = 2 * pi * r * h
            topArea = pi * r^2
        in  sideArea + 2 * topArea

Note `pi` is apparently a global constant. Again, `sideArea` and `topArea`'s
indentation must match in the let definitions, but the extra space after `in` is
cosmetic.

Because `let` is an expression you can use it almost anywhere in code, e.g.

    > 4 * (let a = 9 in a + 1) + 2
    42

or in a lambda like function (not strictly anonymous, but very local):

```
> let square x = x*x in (square 5, square 3, square 2)
(25, 9, 4)
```

`let` can appear like a predicate in a list comprehension, e.g.

    squares :: [Double] -> [Double]
    squares xs = [square | x <- xs, let square = x * x]

`let` also gets used (without `in`) in the interpreter to define a constant or
function whose scope is the entire interactive session. If you include `in`
scope is limited to that line (like in a `.hs` file).

`case` is an alternative to pattern matching. These functions are equivalent

```
head' :: [a] -> a
head' [] = error "Can't call head on empty list"
head' (x:_) = x
```

and 

```
head' :: [a] -> a
head' xs = case xs of [] -> error "Can't call head on empty list"
                      (x:_) -> x
```

but unlike pattern matching, `case` expressions are expressions, and can
therefore be used anywhere, e.g.

    describeList :: [a] -> String
    describeList ls = "List is " ++ case ls of [] -> "empty"
                                               [x] -> "singleton"
                                               xs -> "a longer list"
