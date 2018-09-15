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

## Functor laws

These laws are strong conventions that are _not_ enforced by the compiler, but
apparently make it easier for the programmer to reason about it.

### Law 1: `fmap id = id`

If we map the id function over a functor value, the functor value that we get
back should be the same as the original functor value, e.g.

    > fmap id [1..5]
    [1,2,3,4,5]
    > id [1..5]
    [1,2,3,4,5]

or in the implementation of fmap for Maybe:

    instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing

### Law 2: `fmap (f . g) = fmap f . fmap g`

Composing two functions and then mapping the resulting function over a functor
should be the same as first mapping one function over the functor and then
mapping the other one.

Equivalently `fmap (f . g) x = fmap f (fmap g x)`

### Example violation

Suppose we have a type like this

    data CMaybe a = CNothing | CJust Int a deriving (Show)

It’s a data type that looks much like Maybe a, but the `Just` part holds two
fields instead of one. The first field in the `CJust` value constructor will
always have a type of `Int`, and it will be some sort of counter. The second
field is of type a, which comes from the type parameter, and its type will
depend on the concrete type that we choose for `CMaybe a`.

Suppose `fmap` for this type is implemented like this:

    instance Functor CMaybe where
        fmap f CNothing = CNothing
        fmap f (CJust counter x) = CJust (counter+1) (f x)

Note the counter is incremented in addition to applying it to the generic
value.

This works like this:

    > fmap (++"ha") (CJust 0 "ho")
    CJust 1 "hoha"

This violates the first law:

    > fmap id (CJust 0 "haha")
    CJust 1 "haha"
    > id (CJust 0 "haha")
    CJust 0 "haha"

That's because the "counter" is mutated.

## Applicative functors

### Preamble

If you `fmap` a partially evaluated function on a functor, e.g.

    fmap (+) (Just 3)

Recall that the implementation of `fmap` for `Just` applies the function to the
value inside the just. So this evaluates to:

    Just ((+) 3)

(+3) is a function, wrapped in a Just:

    > :t fmap (+) (Just 3)
    fmap (+) (Just 3) :: Num a => Maybe (a -> a)

More generally, by mapping multiparameter functions over functor values, we get
functor values that contain functions inside them.

    > let a = fmap (+) [1,2,3,4]
    > :t a
    a :: [Integer -> Integer]

We can `fmap` another function over this functor containing function(s)

    > fmap (\f -> f 9) a
    [10,11,12,13]

But what if we want to `fmap` a functor over a functor, e.g. have a functor
value of `Just (3 *)` and a functor value of `Just 5`, and we want to take out
the function from `Just (3 *)` and map it over `Just 5`? We can't do that with
`fmap` because `fmap` expects a function, not a functor containing a function.

That's where `Applicative` and `<*>` come in.

### Applicative

The `Applicative` type class is in `Control.Applicative`. Its definition

```
class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
```

The constraint says that if we want to make a type constructor part of the
Applicative type class, it must be in Functor first. That’s why if we know that
a type constructor is part of the Applicative type class, it’s also in Functor,
so we can use `fmap` on it.

In `pure :: a -> f a`, `f` is an applicative functor.

`pure` should take a value of any type and return an applicative value with
that value inside it. In a sense it takes a value and puts it in some sort of
default (or pure) context—a minimal context that still yields that value

The `<*>` function is more complicated.

```
(<*>) :: f (a -> b) -> f a -> f b
```

It takes an applicative functor (containing a function of type a -> b) and an
applicative functor of type a, and returns an applicative functor of type b.

You can think of the `<*>` function as sort of a beefed-up `fmap`. Whereas
`fmap` takes a function and a functor value and applies the function inside the
functor value, `<*>` takes a functor value that has a function in it and
another functor, and extracts that function from the first functor and then
maps it over the second one.

### Applicative Maybe

```
instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> something = fmap f something
```

The definition for `<*>` extracts the function out of the applicative value and
maps it over the second parameter (which is a functor, from the class
definition).

By definition, we say that if we try to extract a function from a `Nothing`,
the result is a `Nothing`.

```
Prelude> (Just (+3)) <*> (Just 9)
Just 12
```

Extract the function from the applicative value on the left (`(+3)` in this
case) and `fmap` it on the right hand side.

```
Prelude> Just (+3) <*> Just 9
Just 12
```

equivalently

```
Prelude> pure (+3) <*> Just 9
Just 12
```

Apparently "Use pure if you're dealing with Maybe values in an applicative
context (using them with `<*>`); otherwise, stick to Just."

We can chain `<*>` together to use an applicative value function that requires
more than one parameter:

```
pure (+) <*> Just 3 <*> Just 5
Just 8
```

`pure f <*> x <*> y <*> ...` allow us to take a function that expects
parameters that aren't applicative values and use that function to operate on
several applicative values.

`pure f <*> x` equals `fmap f x`. You can see this from the definition of `<*>`
and `pure` for Maybe, but it's generally true because it's one of the
Applicative Laws (see below).

Given this, we can rewrite
```
pure f <*> x <*> y <*> ...
```
as
```
fmap f x <*> y <*> ...
```
and in fact `fmap` is available as an infix operator `<$>` in
`Control.Applicative` so we can write
```
f <$> x <*> y <*> z
```
`<$>` is defined

    (<$>) :: (Functor f) => (a -> b) -> f a -> f b
    f <$> x = fmap f x

> Remember that type variables are independent of parameter names or other
> value names. The f in the function declaration here is a type variable with a
> class constraint saying that any type constructor that replaces f should be
> in the Functor type class. The f in the function body denotes a function that
> we map over x. The fact that we used f to represent both of those doesn’t
> mean that they represent the same thing.

So now we can apply `f` to three applicative values (i.e. values inside a
container) with

```
f <$> x <*> y <*> z
```

if they were simple (non-applicative) values we could have done `f x y z`

### Applicative lists

Definition
```
instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]
```

e.g.

    > pure "Hey" :: [String]
    ["Hey"]
    ghci> pure "Hey" :: Maybe String
    Just "Hey"

The definition of `<*>` for lists means that we're evaluating every pairwise
combination of fs and xs, e.g.

```
> [(\x -> 2*x), (\x -> 3*x)] <*> [4,5]
[8,10,12,15]
```

or more complex

```
> [(+),(*)] <*> [1,2] <*> [3,4]
[4,5,5,6,3,4,6,8]
```

`<*>` is left associative (i.e. `f <*> x <*> y` is `(f <*> x) <*> y`. So this
is equivalent to `[(1+), (2+), (1*), (2*)] <*> [3, 4]`

Or with `<$>`

```
> (\x y -> x + y) <$> [1] <*> [2,3,4]
[3,4,5]
```

Because `<*>` for lists is defined in terms of a comprehension, it can be used
to replace comprehensions, e.g.

```
> [ x*y | x <- [2,5,10], y <- [8,10,11]]
[16,20,22,40,50,55,80,100,110]
```

can be rewritten

```
> (*) <$> [2,5,10] <*> [8,10,11]
[16,20,22,40,50,55,80,100,110]
```

This definition follows the Applicative law we mentioned: `pure f <*> xs`
equals `fmap f xs`.
