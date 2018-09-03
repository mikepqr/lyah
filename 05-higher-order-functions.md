## Chapter 5: Higher-order functions

### Partial evaluation

All functions take only one parameter. Functions that take more than one
parameter are implemented under the hood as a chain of partially evaluated
(curried) functions. Hence the types, e.g.

    replicate' :: Int -> a -> [a]

This function is made up of one function that takes an Int, and returns a
function that takes an `a`, and returns a list of `[a]`.

Or find an individual arrow in a type. The arrow is the single parameter
function. It takes the thing immediately to the left. And returns a function
defined by the next arrow.

So the first arrow in the above definition refers to a function that takes
`Int` and returns a function that maps from `a` to `[a]`.

Consider a function with three parameters

```
multThree :: Int -> Int -> Int -> Int
multThree a b c = a*b*c
```

The type of this function can be rewritten

    multThree :: Int -> (Int -> (Int -> Int))

to make the fact that -> is right-associative clearer. Parens denote one of
these one parameter functions.

Written like that, the first function takes an Int, and returns a function of
type `Int -> (Int -> Int)`, i.e. a function that takes an `Int` and returns a
function of type `Int -> Int`, etc.

To partially evaluate a function, you can do

    > let multTwoNumbersWithNine = multThree 9
    > multTwoNumbersWithNine 2 3
    54

Here the expression `multThree 9` evaluates to a function of type `Int -> (Int
-> Int)`.

What all this means is that, if you give too few parameters to a function that
returns a value, you get back a partially evaluated function, rather than a
value. This is different from python, where you have curry by hand (e.g. using
`functools.partial`), and giving too few parameters results in an error.

Consider this normal function

    compareWithHundred :: Int -> Ordering
    compareWithHundred x = compare 100 x

then

    > compareWithHundred 99
    GT

i.e. 100 is greater than 99. The above definition is _equivalent_ to

    compareWithHundred :: Int -> Ordering
    compareWithHundred = compare 100

Note the type definition is the same. That's because the builtin `compare`

    compare :: Ord a => a -> a -> Ordering

so `compare foo` is of type `a -> Ordering` where `a` is the type of `foo`.

You can't directly print a partially applied function:

    > let multTwoNumbersWithNine = multThree 9  -- this works
    > multThree 9                               -- this doesn't

That's because functions aren't instances of the `Show` type class, i.e. they
don't know how to print themselves.

### Partial evaluation of infix functions (sections)

Infix functions can be partially applied using _sections_. To section an infix,
surround it with parens and supply param on only one side.

    divideByTen :: (Floating a) => a -> a
    divideByTen = (/10)

This is equivalent to the full definition

    divideByTen :: (Floating a) => a -> a
    divideByTen x = x/10

Or

    contains10 :: [Int] -> Bool
    contains10 = (10 `elem`)

### Functions as parameters

The type of a function that takes a function as a parameter is, e.g.

    applyTwice :: (a -> a) -> a -> a
    applyTwice f x = f (f x)

The parens in the type are necessary because -> is right associative.

### zipWith and flip

Two functions in the standard library reimplemented:

Zip two lists together applying a function that takes two parameters to each
tuple (rather than just constructing a list of tuples).

```
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
```

flip the arguments of a function

```
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x
```

Note here the type is takes a function, returns a function. The definition
defines a function and expects three parameters. Calling `flip f` returns the
flipped function, because of partial evaluation.

An equivalent, but more pythonic definition is

```
flip' f = g
    where g x y = f y x
```

This is essentially a translation of

```python
def flip(f):
    def g(x, y):
        return f(y, x)
    return g
```

So the pythonic version returns a function definition. The more idiomatic
Haskell version returns a partially evaluated function (that carries with it
the definition for full evaluation). Kind of.

Examples:

```
> zipWith (/) [1, 2, 3] [3, 3, 3]
[0.3333333333333333,0.6666666666666666,1.0]
> zipWith (flip (/)) [1, 2, 3] [3, 3, 3]
[3.0,1.5,1.0]
```

I'm guessing flip is useful because the kind of implicit partial evaluation
you get when you pass too few parameters is strictly in parameter order.

### map, filter, takeWhile

```
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter predicate (x:xs)
    | predicate x = x : filter p xs
    | otherwise   = filter p xs
```

As with Python, maps and filters can be achieved with list comprehensions. maps
of maps tend to be more readable than nested comprehensions. e.g.

```
> map (+3) [1,5,3,1,6]
[4,8,6,4,9]
> [x + 3 | x <- [1,5,3,1,6]]
[4,8,6,4,9]
```

More map/filter examples. To find the largest number under 100,000 that is
divisible by 3829:

```
largestDivisible :: Integer
largestDivisible = head (filter p [99999,99998..])
    where p x = x `mod` 3829 == 0
```

`head` here is lazy, so we only test values until we find one that satisfies
the predicate.

Here's my implementation of takeWhile

```
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
    | p x = x : takeWhile' p xs
    | otherwise = []
```

Sum of all odd squares less than 10,000:

```
> sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
166650
```

Mapping a function that takes more than one parameter results in a list of
partially evaluated functions, e.g.

```
> let listOfFuncs = map (*) [0..]
> (listOfFuncs !! 4) 5
20
```

### Lambdas

```
largestDivisible :: Integer
largestDivisible = head (filter p [99999,99998..])
    where p x = x `mod` 3829 == 0
```

can be rewritten as

```
largestDivisible = head (filter (\x -> x `mod` 3829 == 0) [99999,99998..])
```

The `\` denotes the beginning of a lambda function. The parameters of the
lambda (if more than one) are separated by spaces after the `\`. The function
body comes after a `->` (not an `=` as with regular function definitions).

Partial evaluation is so easy to do in Haskell that it is generally preferred
to lambdas, e.g. `map (+3) [1,6,3,2]` is preferred to `map (\x -> x + 3)
[1,6,3,2]`.

You can pattern match in lambdas, but only on one pattern. In effect, this
means you can do tuple unpacking:

```
> map (\(a,b) -> a+b) [(1,2), (3,4)]
[3,7]
```

The book argues that this definition

    flip' f = (\x y -> f y x)

or equivalently

    flip' f = \x y -> f y x

is clearer about intended use than the equivalent used above

    flip' f x y = f y x

By returning a lambda expression, i.e. a function, you make clear the expected
use is to produce a new function (which is the most common use case of flip),
rather than to evaluate that new function. The original definition can also be
used to produce a partially evaluated function, but it's less clear from the
definition that that's what you're _supposed_ to do. Or at least I think that's
his argument.

### Folds

The x:xs pattern used a lot above is better down with the `fold` builtin. A
fold takes a binary function (i.e. a function with two parameters), a starting
value (the accumulator) and a list. Lists can be folded from the left or the
right. The binary function is called with the accumulator and the next item on
the list. So

    sum' :: (Num a) => [a] -> a
    sum' xs = foldl (\acc x -> acc + x) 0 xs

or equivalently (although apparently the first one is more idiomatic)

    sum' xs = foldl f 0 xs
        where f a b = a + b

or using sections (partial evaluation of infix functions)

    sum' xs = foldl (+) 0 xs

or, since functions with too few parameters are curried 

    sum' xs = foldl (+) 0 xs

Right fold (`foldr`) is the same except it eats up values from the right, and
the order of params in the binary function is reversed: the current list value
is the _first_ parameter, and the accumulator the second.

The accumulator can be of any type. E.g. if it's a list we can implement `map`

    map' :: (a -> b) -> [a] -> [b]
    map' f xs = foldl (\acc x -> acc ++ [f x]) [] xs

However, because `++` (append) is much slower than `:` (prepend) this is faster
as a right fold:

    map' f xs = foldr (\x acc -> f x:acc) [] xs

Crucially, right folds work on infinite lists but left folds don't?!

Here's `elem` with `foldr`

    elem' :: (Eq a) => a -> [a] -> Bool
    elem' y ys = foldr (\x acc -> if x == y then True else acc) False ys

### foldl1 and foldr1

Just like `foldl` and `foldr` except they assume the starting value of the
accumulator is the first thing in the list, e.g.

    maximum' :: (Ord a) => [a] -> a
    maximum' = foldl1 max

These functions cause runtime errors if called on the empty list.

### More fold examples

    reverse' :: [a] -> [a]
    reverse' = foldl (\x acc -> x : acc) []

or even

    reverse' = foldl (flip (:)) []

This latter works because the lambda function is just like `:` except the
parameters are flipped.

```
product' :: (Num a) => [a] -> a
product' = foldl (*) 1

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x:acc else acc) []

last' :: [a] -> a
last' = foldl1 (\_ x -> x)
```

### Folds and infinite lists

Right folds are essentially doing this (e.g. sum [3,4,5,6])

    3 + (4 + (5 + (6 + 0)))

Left folds do this

    (((0 + 3) + 4) + 5) + 6

Right folds can therefore work on infinite lists if the binary function doesn't
need the second parameter for some reason (e.g. it ignores it, such as `and`
when the first parameter is False).

### scanl, scanr, scanl1 and scanr1

Just like their fold equivalents, except instead of returning the final value
of the accumulator, they return a list of all its intermediate values, e.g.

    > scanl (+) 0 [3,5,2,1]
    [0,3,8,10,11]

scanl appends intermediate accumulators to the result. scanr effectively
prepends them.

    > scanr (+) 0 [3,5,2,1]
    [11,8,3,1,0]

Example: how many elements does it take for the sum of the square root of all
natural numbers to exceed 1000:

    sqrtSums :: Int
    sqrtSums = length takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

The scanl1 returns an infinite list of the accumulated sum of the square roots.
The takeWhile truncates that list. The + 1 corrects for the fact that we want
the sum to _exceed_ 1000.

### Function application with $

Defined

    ($) :: (a -> b) -> a -> b
    f $ x = f x

i.e. it simply applies the function on the left of the $ to the stuff on the
right.

This is syntactic sugar to eliminate parens, e.g.

```
> sum (filter (> 10) (map (*2) [2..10]))
```

is equivalent to

```
sum $ filter (> 10) (map (*2) [2..10])
```

or even

```
sum $ filter (> 10) $ map (*2) [2..10]
```

This is because $ is right associative (while function application with just a
space is left associative). Recall associativity determines how operators of
the same precedence are grouped in the absence of parentheses. So

    f a b c

is equivalent to because it's left associative.

    ((f a) b) c

The other use for $ is to map function application over functions, e.g.

```
> map ($ 3) [(4+), (10*), (^2), sqrt]
[7.0, 30.0, 9.0, 1.73...]
```

the function ($ 3) takes a function and applies it to 3.

### Function composition with .

Defined

    (.) :: (b -> c) -> (a -> b) -> a -> c
    f . g = \x -> f (g x)

Example

    > map (negate . abs) [5, -3, -6]
    [-5, -3, -6]

This could be done with a lambda

    > map (\x -> negate (abs x)) [5, -3, -6]

but composition emphasises the functions (rather than the data) is
syntactically more concise (I think that's the argument).

Composition is right associative, so `(f.f.f) 20` is equivalent to `(f (f (f
20)))`. This is a nice way of chaining together a pipeline of functions
without parens.

### Function composition with multiple parens

    > sum (replicate 5 (max 6.7 8.9))
    44.5

can be rewritten as

    > (sum . replicate 5) (max 6.7. 8.9)

Here the stuff in the first paren is a partially evaluated composition that
includes a curried function.

The book proposes a recipe to rewrite a complicated function with lots of
parens using $ and .

Say you have

    > replicate 2 (product (map (+3) (zipWith max [1,2] [4,5])))
    [56,56]

First replace the deepest paren with a $

    replicate 2 (product (map (+3) $ zipWith max [1,2] [4,5]))

then compose everything else

    replicate 2 . product . map (+3) $ zipWith max [1,2] [4,5]

This is a valid partial evaluation

    replicate 2 . product . map (+3) . zipWith max [1,2]

I don't understand why this is not OK

    replicate 2 . product . map (+3) . zipWith max

### Point-free style

Consider

    sum' :: (Num a) -> [a] -> a
    sum' xs = foldl (+) 0 xs

You get the "point-free" definition by droping the parameter

    sum' = foldl (+) 0

Equivalently, consider

    fn x = ceiling (negate (tan (cos (max 50 x))))

You can't simply drop the paramter here because it's surrounded by parens.
But if you use . then you can:

    fn = ceiling . negate . tan . cos. max 50

Again, I don't understand why 

    fn = ceiling . negate . tan . cos. max

is not OK.
