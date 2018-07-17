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
