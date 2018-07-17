# LYAH

## Chapter 1

To read a Haskell file (`.hs`) into the interpreter do `:h file.hs`.

The else part of an if statement is mandatory. This is part of the requirement
that every expression (or function) returns a value. I.e. "if" is an
expression, not a statement.
    
    doubleSmallNumber x = if x > 100
                            then x
                            else x * 2


It's OK to use apostrophe in Haskell function names. By convention ' denotes a
"strict" version of a function, i.e. one that isn't lazy, or a slightly
modified versio of a function.

Functions cannot begin with an upper case character.

A function that takes no parameters is a "defintion" or "name".

Lists are homogenous: every element is of the same type.

    > let lst = [1, 2, 3]

`let a = 1` in the interpreter is equivalent to just `a = 1` in a Haskell file
read in with `:l file.hs`. (This is according to the book. It doesn't actually
seem to be true.)

It doesn't matter which order functions are defined in a .hs file.

Concatenation of lists is `++`:

    [1, 2, 3] ++ [4, 5, 6] 

Note `1 ++ 2` and `[1] ++ 2` don't work, but `"hello " ++ "world"` does.

A string is a list of characters, so functions that can be applied to lists can
be applied to strings.

A string is wrapped in double quotes. A single character can be a string (i.e.
a single element list of characters), e.g. `"a"`, or a true single character,
which is wrapped in single quotes, `'a'`.

`++` has linear running time in the length of the first list (and second?)

To insert a single element at the start of the list, use `:` (aka the "cons"
operator)

    1:[2, 3, 4, 5]

To insert a single element to the back of the list, you need to wrap it in
brackets and use `++`.

You can prepend to the empty list to make a single element list:

    > 1:[]
    [1]

This is actually how `[1, 2, 3]` works: it's syntactic sugar for `1:2:3:[]`.

Use `!!` to index a list (0-index):

    > [1, 2, 3] !! 1
    2

Lists within lists can be different lengths, but they can't contain different
types.

Lists compare like in Python. First non-equal element determines winner:

    > [1, 2] > [1, 3]
    False

The empty list is less than all non-empty lists, so if they lists differ in
length you get

    > [1, 2] > [1]
    True

Comments begin with `--`.

Other list operations (all of which raise an error at runtime if applied to the
empty list.)

    > head [1, 2, 3]
    1
    > tail [1, 2, 3]  -- all except head
    [2, 3]
    > last [1, 2, 3]
    3
    > init [1, 2, 3]  -- all except last
    [1, 2]

These list functions do work on the empty list

    > length [1, 2, 3]
    3
    > null [1, 2, 3]
    False
    > null []
    True
    > reverse [1, 2, 3]
    [3, 2, 1]
    > take 2 [1, 2, 3]
    [1, 2]

If you try to take more elements than are in a list you get the entire list (so
applied to the empty list you get the empty list for all values of first
argument).

    > drop 3 [8, 4, 2, 1, 5, 6]
    [1, 5, 6]

If you try to drop more elements than are in the list, you get the empty list.

    > maximum [8, 4, 2, 1, 5, 6]
    8
    > minimum [8, 4, 2, 1, 5, 6]
    1
    > sum [8, 4, 2, 1, 5, 6]
    26
    > product [8, 4, 2, 1, 5, 6]
    1920

Division:

    > 7/8
    0.875
    > div 7 8
    0
    > 7 `div` 8  -- functions with two arguments can be called infix-style
    > 7 `mod` 3
    1

Membership of list

    > 8 `elem` [8, 4, 2, 1, 5, 6]
    True
    > 9 `elem` [8, 4, 2, 1, 5, 6]
    False
    > elem 9 [8, 4, 2, 1, 5, 6]
    False

Enumerations

    > [1..10]
    [1,2,3,4,5,6,7,8,9,10]
    > ['a'..'k']
    "abcdefghijk"
    > [1,3..10]
    [1,3,5,7,9]
    > [10,9..1]
    [10,9,8,7,6,5,4,3,2,1]

Infinite lists

    > take 10 [1,3..]  -- no termination value in enumeration
    [1,3,5,7,9,11,13,15,17,19]
    > take 10 (cycle [1,2,3])
    [1,2,3,1,2,3,1,2,3,1]
    > take 10 (repeat 5)
    [5,5,5,5,5,5,5,5,5,5]
    > replicate 10 5  -- alternative to above, no lazy evaluation
    [5,5,5,5,5,5,5,5,5,5]

Comprehensions

```
> [x*2 | x <- [1..10]]
[2,4,6,8,10,12,14,16,18,20]
```

(*Draw* elements from [1..10], *bind* to x. Part before the pipe is the
*output*.)

```
> [x*2 | x <- [1..10], x*2 > 12]
[14,16,18,20]
```

`x*2 > 12` is the *predicate*. There can be multiple predicates, each separated
by commas.

Some tests:

    > odd 7
    True
    > even 6
    True

FizzBuzz

    fizzBuzz xs = [if x `mod` 5 == 0 && x `mod` 3 == 0 then "FizzBuzz"
                    else if x `mod` 3 == 0 then "Fizz"
                    else if x `mod` 5 == 0 then "Buzz"
                    else show x | x <- xs]

Inequality is `/=`, not `!=`, e.g.

    > 5 /= 10
    True

You can draw multiple values from a list in a comprehension. Every possible
combination is yielded (cartesian product):

    > [[x, y] | x <- [1, 2, 3], y <- [4, 5, 6]]
    [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]

Comprehension version of length function

    length' xs = sum [1 | _ <- xs]

Strip non-upper-case

    stripNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

    > stripNonUppercase "Hello World"
    "HW"

Tuples are surrounded by parens. They can contain distinct types, unlike lists.
And each set of tuples of a particular length and content types is itself a
type. So `(1, 2)` is a tuple. So is `(1, "two")`, but they are not the same
type. Same goes for `(1, 2, 3)`. 

Tuples are fixed size. No append or mutate. No single element tuples (but the
empty tuple is allowed).

Not sure how you index into tuples! If `x = (1, 2)`, `x !! 0` doesn't work.
`fst` and `snd` give the first and second elements of a _pair_ (i.e. a tuple
with two elements).

`zip` takes two lists and creates a list of pairs:

```
> zip [1, 2, 3] [5, 5, 5]
[(1, 5), (2, 5), (3, 5)]
> zip [1, 2, 3, 4] [5, 5, 5]
[(1, 5), (2, 5), (3, 5)]
> zip [1..] [5, 5, 5]
[(1, 5), (2, 5), (3, 5)]
```

## Chapter 2: Types

Get the type of an expression in ghci with `:t`

    > :t 'a'
    'a' :: Char
    > :t "a"
    "a" :: [Char]
    > :t True
    True :: Bool
    > :t (True, 'a')
    (True, 'a') :: (Bool, Char)
    > :t ('a', 'a', 'a')
    ('a', 'a', 'a') :: (Char, Char, Char)
    > :t (
    > :t 7+2 == 8+1
    7+2 == 8+1 :: Bool

The `::` operator can be read "has type of".

Functions can optionally have an explicit type declaration.

    stripNonUppercase :: [Char] -> [Char]
    stripNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

i.e. this function has a type (is a function that) takes an array of characters
and returns an array of characters.

    addThreeNumbers :: Int -> Int -> Int -> Int
    addThreeNumbers a b c = a + b + c

The types of the parameters and return value of a function are _all_ separated
with `->`.

`:t` works on functions (whether or not they are annotated):

    > :t stripNonUppercase
    stripNonUppercase :: [Char] -> [Char]

Basic types:

 - `Int` is a bounded integer (-2^63 to 2^63 - 1)
 - `Integer` is an unbounded integer
 - `Float` is a single precision float
 - `Double`
 - `Bool`
 - `Char` and `[Char]` (string)
 - all possible tuples and lists, e.g.
    
       > :t [1, 2]
       [1, 2] :: Num a => [a]
       > :t (1, 2)
       (1, 2) :: (Num a, Num b) => (a, b)

_Type variables_ are placeholders for a type. Above they mean `[1, 2]` is a
list of `a`, and `a` is a `Num` (which includes all the number types above).

Function types can have type variables. Such functions are called polymorphic.
E.g. recall `head`. It takes a list of any type, and returns a single element
of that type. Thus

    > :t head
    head :: [a] -> a
    > :t fst
    fst :: (a, b) -> b

A _type class_ is an interface that defines a behavior, and a type is an
instance of that class.

    > :t (==)
    (==) :: Eq a => a -> a -> Bool

Everything before the `=>` symbol is called a _class constraint_. In this case,
it's telling us that the type `a` (i.e. the type of the two arguments to `==`)
must be an instance of the `Eq` class.

  - `Eq` types (which is pretty much everything except io and functions)
    implement `==` and `/=`

        > :t (/=)
        (/=) :: Eq a => a -> a -> Bool
           
  - `Ord` types (again everything except io and functions) can be ordered. They
    implement `>`, `<`, `>=`, `<=` and `compare`

        > :t compare
        compare :: Ord a => a -> a -> Ordering

    Compare returns an `Ordering`, which are printed as `GT` `LT` or `EQ`

  - `Show` types can be represented as strings, e.g. they implement `show`

        > :t show
        show :: Show a => a -> String

  - `Read` types can be read from strings using the `read` function

        > read "5" - 2
        3
        > :t read
        read :: Read a => String -> a

    Note that you can't do `read "5"` because there isn't enough information in
    that expression to infer the type `a` in the type of `read`. But you can
    add a type annotation:

        > read "5" :: Double
        5.0
        Prelude> read "5" :: Int
        5

    `read "5" - 2` allowed Haskell to infer the type the `read` function should
    return. It can do this with lists too:

        > [read "1", 2, 3]
        [1, 2, 3]

  - `Enum` types are ordered sequence types, which have defined successors and
    predecessors. `Char`, `Int` (and `Float`) are all of this type
    class

        > succ 'a'
        b
        > succ 1
        2
        > succ 1.1
        2.1

  - `Bounded` types have bounds and implement `minBound` and `maxBound`

        > minBound :: Int
        -9223372036854775808

    `minBound` and `maxBound` are kind of like polymorphic constants. By
    providing the annotation you define `a` in their type.

        > :t minBound
        minBound :: Bounded a => a

    ("`minBound` returns an `a`, where `a` must be in the `Bounded` type
    class".)

  - `Num` are the numbers. They implement `+`, `*`, etc. `Num` types must also
    also members of `Eq` and `Show`.
  
        > :t (+)
        (+) :: Num a => a -> a -> a
          
    Whole numbers are polymorphic constants like `minBound`

        > 2 :: Int
        2
        > 2 :: Float
        2.0

  - `Floating` types include `Float` and `Double`.

        > :t sin
        sin :: Floating a => a -> a
        > sin 1
        0.8414709848078965

    Recall 1 is a polymorphic constant. Type inference results in it being
    interpreted as a `Float` when `sin` is applied

  - `Integral` includes `Int` and `Integer`.

        > :t fromIntegral
        fromIntegral :: (Integral a, Num b) => a -> b

    This is saying that from integral takes a type `a` which must be integral,
    and returns a type `b` which must be a `Num`. So

        > x = fromIntegral 1 :: Double
        > :t x
        x :: Double

    This function is useful when you want to operate on floats and the results
    of functions that yield integers. E.g. `length` returns an Int, so we can't
    do `length [1, 2, 3] * 4.0` but we can do

        > fromIntegral (length [1, 2, 3]) * 4.0
        12.0

A type can be an instance of many type classes. A type class can have many type
instances. Some type classes have membership prerequisites, e.g. to be a member
of `Ord` a type must first be a member of `Eq`.

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

## Chapter 4: Recursion

> Recursion is important in Haskell because, unlike with imperative languages,
> you do computations in Haskell by declaring what something is rather than
> specifying how you compute it. That’s why Haskell isn’t about issuing your
> computer a sequence of steps to execute, but rather about directly defining
> what the desired result is, often in a recursive manner.

Recall `max` is a builtin that returns the larger of its two arguments. Given
that, here's a recursive definition of `maxmium'` that works on longer
sequences. (`maximum` is already builtin. This is just an example.)

    maximum' :: (Ord a) => [a] -> a
    maximum' [] = error "empty list"
    maximum' [x] = x
    maximum' (x:xs) = max x (maximum' xs)

The parens around the class constraint `Ord a` seem to be optional. The base
cases and the recursive step are defined here with pattern matching. `x:xs` is
a commmon way to pattern match on the head and tail of a list.

Now lots more builtins reimplemented:

  - `replicate`

        replicate' :: Int -> a -> [a]
        replicate' n x
            | n <= 0 = []
            | otherwise = x : replicate' (n-1) x

    Note we use guards here rather than pattern matching because we need to
    test for a boolean condition.

  - `take`. I wrote it like this

        take' :: Int -> [a] -> [a]
        take' n xs
            | n <= 0 = []
            | n == 1 = [head xs]
            | otherwise = head xs : take' (n-1) (tail xs)

    The book does it like this

        take'' :: Int -> [a] -> [a]
        take'' n xs
            | n <= 0 = []
        take'' _ [] = []
        take'' n (x:xs) = x : take'' (n-1) xs

    The recursive step says "taking n elements from a list is the same as
    prepending the first element of that list to what you get when you take n-1
    elements from the list".

  - `reverse`

        reverse' :: [a] -> [a]
        reverse' [] = []
        reverse' (x:xs) = reverse' xs ++ [x]

    Note we have to join the reversed tail and the head with `++`. You can't
    append to a list with `:`.

  - `repeat` returns an infinite list of its argument

        repeat' :: a -> [a]
        repeat' x = x:repeat' x

    Note there is no base case, because this is an infinite list.

 - `zip`

        zip' :: [a] -> [b] -> [(a, b)]
        zip' _ [] = []
        zip' [] _ = []
        zip' (x:xs) (y:ys) = (x,y):zip' xs ys

    Note there are two empty list base bases.

 - `elem` (check for membership)

        elem' :: (Eq a) => a -> [a] -> Bool
        elem' a [] = False
        elem' a (x:xs)
            | a == x = True
            | otherwise = a `elem'` xs

Recall quicksort algorithm. To sort:
 
 - pick a pivot, e.g. the first element
 - create new list which is a
    - quicksorted list of all elements smaller than the pivot
    - the pivot
    - quicksorted list of all elements larger than the pivot

This can be implemented like this

    quicksort :: (Ord a) => [a] -> [a]
    quicksort [] = []
    quicksort (x:xs) =
        let smallerOrEqual = [a | a <- xs, a <= x]
            larger = [a | a <- xs, a > x]
        in  quicksort smallerOrEqual ++ [x] ++ quicksort larger

`smallerOrEqual` and `larger` are locally defined (`let`) lists constructed by
comprehensions with predicates that correspond to the elements that are smaller
or larger than the first element of the list. The last line implements the
recursive algorithm.

In python this would look like

```python
def quicksort(xs):
    if len(xs) == 0:
        return []
    else:
        head, tail = xs[0], xs[1:]
        smaller_equal = [x for x in tail if x <= head]
        larger = [x for x in tail if x > head]
        return quicksort(smaller_equal) + [head] + quicksort(larger)
```

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
