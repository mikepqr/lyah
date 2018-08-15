## Chapter 6: Modules

### Imports

Imports must happen before function definitions, so by convention happen at the
top of a file.

`import modulename` is the equivalent of `from modulename import *` in python.

`import qualified modulename` is the equivalent of `import modulename` in
python.

`import qualified modulename as m` is the equivalent of `import modulename as
m` in python.

`import modulename (foo,bar)` is the equivalent of `from modulename import foo,
bar`.

`import modulename hiding foo` means import everything except foo.

`:m + modulename` to import into the interpreter. `:m + modulename1
modulename2` to import several modules.

### Standard library examples

To search the standard library use [Hoogle](https://www.haskell.org/hoogle/).

`Data.List.nub` removes duplicates in a list, so e.g.

```
import Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub
```

#### Word counts

`Data.List.words` splits a string on whitespace. 

`Data.List.sort` does what you'd expect to a list.

`Data.List.group` groups duplicates in a *sorted* list into sublists, e.g.

```
> group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
[[1,1,1,1],[2,2,2,2],[3,3],[2,2,2],[5],[6],[7]]
```

Hence,

```
wordNums :: String -> [(String,Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words
```

which does

```
> wordNums ["wa","wa","wee","wa"]
[("wa",3),("wee",1)]
```

#### Is list sublist of list

`Data.List.tails` returns all the suffixes of a list, e.g.

```
> tails "party"
["party","arty","rty","ty","y",""]
```

`Data.List.isPrefixOf` says whether a list is a prefix of another, e.g.

```
> "hawaii" `isPrefixOf` "hawaii joe"
True
> "haha" `isPrefixOf` "ha"
False
```

`Data.List.any` takes a predicate and a list, and it tells us if any element
from the list satisfies the predicate:

```
> any (> 4) [1,2,3]
False
> any (=='F') "Frank Sobotka"
True
```

All together

```
isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)
```

(This function is already in Data.List as `isInfixOf`)

#### Data.Char

`Data.Char.ord` returns unicode location of a character. `chr` reverses this.

So to encode with Caeser ciper

```
import Data.Char

encode :: Int -> String -> String
encode offset msg = map (\c -> chr $ ord c + offset) msg
```

The function in parens can also be written `(chr . (+ offset) . ord)` with
composition.

`Data.Char.digitToInt` converts a character in the range '0' to '9' or 'A' to
'F' to an integer in the range 0 to 9.

### strict foldl

foldl defers computation until the entire list has been consumed. This means it
can build up a very large stack that can overflow:

```
> foldl (+) 0 (replicate 1000000 1)
*** Exception: stack overflow
```

This doesn't actually happen on my machine, but the computation is very slow.

`Data.List.foldl'` is the strict version that evaluates greedily.

### Maybe, Just and Nothing

The `Data.List.find` function takes a predicate and a list and returns the
first element that satisfies the predicate. So you might think:

```
find :: (a -> Bool) -> [a] -> a
```

but what if no element satisfies the predicate. To capture this possibility
Haskell uses Maybes.

```
find :: (a -> Bool) -> [a] -> Maybe a
```

A value `Maybe a` is sort of like a list of a's that can have no elements. We
use it to represent possible failure. If we need to manually make this empty
value, we do `Nothing`. If we need to manually make a `Maybe a` we do `Just`,
so:

```
> :t Nothing
Nothing :: Maybe a
> :t Just 5
Just 5 :: Num a => Maybe a
```

So if `find` finds an element, it returns that element wrapped in `Just`:

```
> find odd [2,4,6,8,9]
Just 9
> find (=='z') "mjolnir"
Nothing
```

Example use of find: find the first integer whose digits sum to 40. This
function takes a number and returns the sum of its digits:

```
import Data.Char

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show
```

Given that, then

```
sumTo40 :: Maybe Int
sumTo40 = find (\x -> digitSum x == 40) [1..]
```

Note the type!

### Association lists

~= Python dictionaries by hand.

```
phoneBook =
    [("betty", "555-2938")
    ,("bonnie", "452-2928")
    ,("patsy", "493-2928")
    ,("lucille", "205-2928")
    ,("wendy", "939-8282")
    ,("penny", "853-2492")
    ]
```

To look up a value by key in this data structure, this will work:

```
findKey :: (Eq k) => k -> [(k, v)] -> v
findKey key xs = snd . head . filter (\(k, v) -> key == k) $ xs
```

(Still note completely sure why that `$` is necessary.)

But we'll get a runtime error if the key is missing. To handle that possibility
explicitly in the `findKey` definition:

```
findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs)
    | key == x  = Just v
    | otherwise = findKey key xs
```

This works with explicit recursion. But according to the book "usually better
to use folds for this standard list recursion pattern, rather than explicitly
writing the recursion, because they’re easier to read and identify", i.e.

```
findKey key xs = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing xs
```

The accumulator here is initialized to `Nothing`, then we scan the entire list
setting the accumulator to the value of any keys that match the one we're
searching for (if any). Because we're scanning from the right, the final time
this happens this will give us the first occurence of the key. Seems
inefficient!

In fact these are a real builtin datatype in the standard library, so you don't
have to implement any of this.

```
import qualified Data.Map as Map
```

then

```
> Map.fromList [(3,"shoes"),(4,"trees"),(9,"bees")]
fromList [(3,"shoes"),(4,"trees"),(9,"bees")]
```

When a map from Data.Map is displayed on the terminal, it’s shown as fromList
and then an association list that represents the map, even though it’s not a
list anymore.

`Map.lookup` does lookups by key. `Map.insert` takes a key, a value, and a map,
and returns a new map with the new key/value. `Map.size` takes a map and
returns its size. `Map.map` takes a function and a map, and applies that
function to each value in the map, and returns the new map.

`Map.fromListWith` acts like `fromList`, but instead of discarding
duplicate keys, it uses a function supplied to it to decide what to do with
them. e.g. keep the largest:

```
Map.fromListWith max [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]
fromList [(2,100),(3,29),(4,22)]
```

### User modules

To create a `Geometry` module start `Geometry.hs` like this, including a list
of the functions it exports.

```
module Geometry
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea
, cuboidArea
, cuboidVolume
) where
...
```

Or you can have a folder Geometry containing Sphere.hs and then do e.g.

```
module Geometry.Sphere
( volume
, area
) where

volume :: Float -> Float
volume radius = (4.0 / 3.0) * pi * (radius ^ 3)

area :: Float -> Float
area radius = 4 * pi * (radius ^ 2)
```
