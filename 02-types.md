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
