# Applicative functors

## IO

`IO` is an instance of `Functor`, i.e. it can be thought of as a container or
context for a value, and a function can applied to the value while retaining
that context with `fmap`.

    instance Functor IO where
        fmap f action = do
            result <- action
            return (f result)

Recall `do` returns the last value in the block and `return` is an IO action
that does nothing and yields a value as its argument.

So instead of

    main = do line <- getLine
              let line' = reverse line
              putStrLn $ "You said " ++ line' ++ " backwards!"

You can do

    main = do line <- fmap reverse getLine
              putStrLn $ "You said " ++ line ++ " backwards!"


If you ever find yourself binding the result of an I/O action to a name, only
to apply a function to that and call that something else, consider using fmap.
If you want to apply multiple functions to some data inside a functor, you can
declare your own function at the top level, make a lambda function, or,
ideally, use function composition:

## Functions as functors

The function type `r -> a` can be written as `(->) r a`

Recall that for a type to be a functor, its constructor must take exactly one
parameter. `fmap`'s type

    fmap :: (Functor f) => (a -> b) -> f a -> f b

where f is a type. So if we're making `(->) r` a functor `fmap` will have type

    fmap :: (a -> b) -> (->) r a -> (->) r b

or in other words

    fmap :: (a -> b) -> (r -> a) -> (r -> b)

The definition is

    instance Functor ((->) r) where
        fmap f g = (\x -> f (g x))

(here `f` is a function, not a type.)


So `(->) r` can be (and is) a functor.

    instance Functor ((->) r) where
        fmap f g = (\x -> f (g x))

This is essentially function composition. We pipe the output of r -> a into the
input of a -> b to get a function r -> b, which is exactly what function
composition is all about. Here’s another way to write this instance:

    instance Functor ((->) r) where
        fmap = (.)

then

```
> :t fmap (*3) (+100)
fmap (*3) (+100) :: Num b => b -> b
> fmap (*3) (+100) 1
303
```

e.g. Using `fmap (*3)` on `(+100)` will create another function that acts like
`(+100)`, but before producing a result, `(*3)` will be applied to that result.

### Lifting a function

`fmap` takes two parameters (a function and a functor), but if you only supply
one you get a function that can be applied to a functor.

```
> :t fmap (*2)
fmap (*2) :: (Num a, Functor f) => f a -> f a
> :t fmap (replicate 3)
fmap (replicate 3) :: (Functor f) => f a -> f [a]
```

The first one says: The expression `fmap (*2)` is a function that takes a
functor f over numbers and returns a functor over numbers. That functor can be
a list, a Maybe, an Either String, or anything else.

You can think of fmap in two ways:

 - As a function that takes a function and a functor value and then maps that
   function over the functor value (previous section)

 - As a function that takes a function and lifts that function so it operates
   on functor values (this section)
