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
