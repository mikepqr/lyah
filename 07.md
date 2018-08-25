# Chapter 7. Making Our Own Types and Type Classes

## Data keyword

    data Bool = False | True

The part before the equal sign denotes the type, which in this case is Bool.

When declaring a data type, the part before the = is the type constructor, and
the constructors after it (possibly separated by | characters) are value
constructors.

The value constructors specify the different values that this type can have. So
we can do

    data Shape = Circle Float Float Float | Rectangle Float Float Float Float

where the three floats of the circle define its center and radius, and the four
floats of the rectangle define its corners.

Circle and Rectangle here are called value constructors.

    > :t Circle
    Circle :: Float -> Float -> Float -> Shape

Note circle is not a type, any more than True is. Shape is a type. Here's a
kind of polymorphic function:

    area :: Shape -> Float
    area (Circle _ _ r) = pi * r ^ 2
    area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

If we try to just print out Circle 10 20 5 from the prompt, we’ll get an error.
That’s because Haskell doesn’t know how to display our data type as a string.
We need to make the Shape type part of the Show class, like this:

    data Shape = Circle Float Float Float | Rectangle Float Float Float Float
         deriving (Show)

Then this works

    > Circle 10 20 5
    Circle 10.0 20.0 5.0

Let's make the points in shapes a type

    data Point = Point Float Float deriving (Show)
    data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

Notice that when defining a point, we used the same name for the data type and
the value constructor. This has no special meaning, although it’s common if
there’s only one value constructor. 

If we put all this in a module we can export it like this

    module Shapes
    ( Point(..)
    , Shape(..)
    , area
    ) where

The `Shape(..)` stuff exports all the constructors of a type. It's the same as
writing `Shape (Rectangle, Circle)` but shorter. We don't have to do this if we
want to hide the ability to use these constructors.

## Records

These are like data types but with named fields, e.g.

    data Person = Person { firstName :: String
                         , lastName :: String
                         , age :: Int
                         , height :: Float
                         , phoneNumber :: String
                         , flavor :: String } deriving (Show)

The main benefit of using this syntax is that it creates functions that look up
fields in the data type. By using record syntax to create this data type,
Haskell automatically makes these functions: firstName, lastName, age, height,
phoneNumber, and flavor.

Apparently this means you can't have record types with fields with the same
name?!

## Type parameters

E.g. Circle and Rectangle return a value of type Shape.

    data Shape = Circle Float Float Float | Rectangle Float Float Float Float

But if the data keyword is followed by a name and a type parameter, the name is
a type constructor, not a value constructor, e.g.

    data Maybe a = Nothing | Just a

Here a is a type parameter. `Maybe` alone is not a type. `a` must be a
particular type, that is the type that values of this type can take when
they're not Nothing.

Types like `Shape` or `Maybe Int` are concrete types. All values have a
concrete type.

We usually use type parameters when the type that’s contained inside the data
type’s various value constructors isn’t really that important for the type to
work.

## Vector type
    
We don’t put type constraints into data declarations, even if it seems to make
sense. You’ll need to put them into the function type declarations either way.
So:

```
data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = i*l + j*m + k*n

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i*m) (j*m) (k*m)
```

## Type classes

If a data type derives from a type class, Haskell is able to fill in some of
the behavior. E.g. for a record

    data Person = Person { firstName :: String
                         , lastName :: String
                         , age :: Int
                         } deriving (Eq)

two variables of type Person while be equal if they were build with the same
value constructor (in this case there is only one), and the comparing each of
the fields. The fields must be part of `Eq` for this to work, which `String` and
`Int` are.

Deriving `Read` and `Show` do what you'd expect (make it possible to read a
variable of this type from a string, or write a variable of this type as a
string).

Deriving `Ord` gives you a type in which variables dervied from the different
value constructors have an ordering, e.g.

    > data Bool = False | True deriving (Ord)
    > True > False
    True

For this reason `Nothing` is less than all `Just` values.

Types deriving `Bounded` support `maxBound` and `minBound`, e.g.

    > data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
                  deriving (Eq, Ord, Show, Read, Bounded, Enum)
    > minBound :: Day
    Monday

Types deriving `Enum` support `succ` and `..`

    > [Thursday .. Sunday]
    [Thursday,Friday,Saturday,Sunday]

## Synonyms

`String` and `[Char]` are synonyms. That is implemented with

    type String = [Char]

They don't really do anything except provide more descriptive names to people
reading the code, e.g.

    type PhoneNumber = String
    type Name = String
    type PhoneBook = [(Name, PhoneNumber)]

Note that `type` is for declaring synonyms, not types!

Synonyms can have type parameters, e.g.

    type AssocList k v = [(k, v)]

`AssocList` is a type constructor that takes two types and produces a concrete
type—for instance, `AssocList Int String`.

You can partially apply type parameters, e.g.

    type IntMap v = Map Int v

is equivalent to

    type IntMap = Map Int

Either way, the IntMap type constructor takes one parameter, and that is the
type of what the integers will point to.

Type synonyms (and types generally) can be used only in the type portion of
Haskell. Haskell’s type portion includes data and type declarations, as well as
after a :: in type declarations or type annotations.

## Either, Left, Right

Another cool data type that takes two types as its parameters is the Either a b
type. This is roughly how it’s defined:

    data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

Kind of like `Nothing` has type `Maybe a`, `Left True` has type `Either Bool b`

`Maybe`s are conventionally used as the return value of functions that can fail
in one way (in which case `Nothing` indicates that failure).

But if the function can fail in several ways, or information about the nature
of the failure is useful to the caller, `Either a b` is used. 

In this case, by convention, `a` is a type that can tell us something about the
possible failure, and `b` is the type of a successful computation. Hence,
errors use the Left value constructor, and results use Right.

For example: Each locker has a code combination. When students need to be
assigned a locker, they tell the locker supervisor which locker number they
want, and he gives them the code. However, if someone is already using that
locker, the student needs to pick a different one. Set up some types:

    import qualified Data.Map as Map

    data LockerState = Taken | Free deriving (Show, Eq)

    type Code = String

    type LockerMap = Map.Map Int (LockerState, Code)

Next, we’ll make a function that searches for the code in a locker map. We’ll
use an `Either String Code` type to represent our result, because our lookup
can fail in two ways: The locker can be taken, in which case we can’t tell the
code, or the locker number might not exist. If the lookup fails, we’re just
going to use a String to indicate what happened.

    lockerLookup :: Int -> LockerMap -> Either String Code
    lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
        Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
                                then Right code
                                else Left $ "Locker " ++ show lockerNumber
                                            ++ " is already taken!"

## Recursive data structures

Recall that `[5]` is syntactic sugar for `5:[]`, where `:` is the cons
operator. Cons is right associative (i.e. read a series of them from left to
right, `3:4:5:6:[]` == `[3,4,5,6]`.

You could implement a list data type like this

    data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

We can define functions to be automatically infix by naming them using only
special characters. We can also do the same with constructors, since they’re
just functions that return a data type. There is one restriction however: Infix
constructors must begin with a colon. So we can replace `Cons` with `:-:` like
this:

    infixr 5 :-:
    data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

The `infix` statement defines an operators "fixity", i.e. precedence and
whether it's left/right associative.

So we can define a ^++ infix function (operator) to concatenate our List type

    infixr 5  ^++
    (^++) :: List a -> List a -> List a
    Empty ^++ ys = ys
    (x :-: xs) ^++ ys = x :-: (xs ^++ ys)

## Binary search trees

Here’s what we’re going to say: A tree is either an empty tree or it’s an
element that contains some value and two trees. This is a good fit for
"algrebraic data types".

    data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

So then

    singleton :: a -> Tree a
    singleton x = Node x EmptyTree EmptyTree

and

    treeInsert :: (Ord a) => a -> Tree a -> Tree a
    treeInsert x EmptyTree = singleton x
    treeInsert x (Node a left right)
        | x == a = Node x left right
        | x < a  = Node a (treeInsert x left) right
        | x > a  = Node a left (treeInsert x right)

and

    treeElem :: (Ord a) => a -> Tree a -> Bool
    treeElem x EmptyTree = False
    treeElem x (Node a left right)
        | x == a = True
        | x < a  = treeElem x left
        | x > a  = treeElem x right

Instead of manually creating one (although we could), we’ll use a fold to build
a tree from a list. Remember that pretty much everything that traverses a list
one item at a time and returns a value can be implemented with a fold! We’re
going to start with the empty tree and then approach a list from the right and
insert element after element into our accumulator tree.

    > let nums = [8,6,4,1,7,3,5]
    > let numsTree = foldr treeInsert EmptyTree nums

Remember that pretty much everything that traverses a list one item at a time
and returns a value can be implemented with a fold!

## Creating instances of type classes

A quick type class recap: Type classes are sort of like interfaces. A type
class defines some behavior (such as comparing for equality, comparing for
ordering, and enumeration). Types that can behave in that way are made
instances of that type class. The behavior of type classes is achieved by
defining functions or just type declarations that we then implement. So when we
say that a type is an instance of a type class, we mean that we can use the
functions that the type class defines with that type.

Eq is for values that can be equated. It defines the functions == and /=. If we
have the type Car and comparing two cars with the equality function == makes
sense, then it makes sense for Car to be an instance of Eq.

This is how the Eq class is defined in the standard library:

    class Eq a where
        (==) :: a -> a -> Bool
        (/=) :: a -> a -> Bool
        x == y = not (x /= y)
        x /= y = not (x == y)

Making a type an instance of Eq by hand:

    data TrafficLight = Red | Yellow | Green

    instance Eq TrafficLight where
        Red == Red = True
        Green == Green = True
        Yellow == Yellow = True
        _ == _ = False

The function bodies for the functions that Eq defines are defined in terms of
mutual recursion. This means that we only need to define one of them in our
TrafficLight instance of the class. If Eq were defined like this TrafficLight
would have to define both:

    class Eq a where
        (==) :: a -> a -> Bool
        (/=) :: a -> a -> Bool

So an instance of Eq must define either == or /=. That’s called the minimal
complete definition for the type class—the minimum of functions that we must
implement so that our type can behave as the class advertises.

Note we can get all this behavior simply by deriving Show. The above is an
example.

## Creating instances of type classes with parameters

e.g.

    instance Eq (Maybe m) where
        Just x == Just y = x == y
        Nothing == Nothing = True
        _ == _ = False

but really we should require that m is an instance of Eq, i.e.

    instance (Eq m) => Eq (Maybe m) where

...so that the comparisons work.

## Subclassing

In the stdlib definition of Num

    class (Eq a) => Num a where

The class constraint says that in order for `a` to be `Num` is must first be
`Eq`. This is subclassing.

Usually class constraints in class declarations are used for making a type
class a subclass of another type class, and class constraints in instance
declarations are used to express requirements about the contents of some type.

If you want to see what the instances of a type class are, just type :info
YourTypeClass in GHCi. For instance, typing :info Num will show which functions
the type class defines, and it will give you a list of the types in the type
class

## YesNo example

Python etc. assigns truthiness to non-boolean types. Let's implement that with
classes:

    class YesNo a where
        yesno :: a -> Bool

    instance YesNo Int where
        yesno 0 = False
        yesno _ = True

    instance YesNo [a] where
        yesno [] = False
        yesno _ = True

    instance YesNo Bool where
        yesno = id

`id` is the identity function.

```
instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True
```

Now let’s make a function that mimics the if statement, but that works with
YesNo values.

    yesnoIf :: (YesNo y) => y -> a -> a -> a
    yesnoIf yesnoVal yesResult noResult =
        if yesno yesnoVal
            then yesResult
            else noResult

    > yesnoIf [] "YEAH!" "NO!"
    "NO!
    > yesnoIf [2,3,4] "YEAH!" "NO!"
    "YEAH!"

## Functor

The Functor type class is for things that can be mapped over. 

Functors are typically some kind of container type that provide a way to apply
functions to their contents.

Its definition is

    class Functor f where
        fmap :: (a -> b) -> f a -> f b

(i.e. it defines one function but doesn't implement it.)

This is a bit different. Recall

    class YesNo a where
        yesno :: a -> Bool

`a` there is a concrete type. But in the type definition of `fmap` `f` is a
type constructor (i.e. a function that takes a type as a parameter and returns
a concrete type).

fmap takes a function from one type to another and a functor value applied with
one type and returns a functor value applied with another type.

map is just a fmap that works only on lists:

    map :: (a -> b) -> [a] -> [b]

List is a member of Functor

    instance Functor [] where
        fmap = map

Since for lists, fmap is just map, we get the same results when using these
functions on lists:

```
> fmap (*2) [1..3]
[2,4,6]
> map (*2) [1..3]
[2,4,6]
```

## Maybe as Functor

List is an empty box that can have nothing or something in it. In that sense
`Maybe` is the same deal: it's a box that can have nothing (in which case it
has the value `Nothing`) or it can contain an item. Types that act like this
can be functors.

Here's how Maybe is a functor:

    instance Functor Maybe where
        fmap f (Just x) = Just (f x)
        fmap f Nothing = Nothing

i.e. fmap returns `Just (f x)` or `Nothing` when `f` is applied to a Maybe type.

    > fmap (++ " world") (Just "hello")
    Just "hello world"
    > fmap (+2) Nothing
    Nothing

Note we wrote `instance Functor Maybe where` without the type parameter, not
`instance Functor (Maybe a) where`, which is what we did for `Maybe` and
`YesNo`. Functor expects a type constructor (i.e. a function that returns a
type), not a type. We know this from the type of fmap

    fmap :: (a -> b) -> f a -> f b

for the Maybe instance, this type doesn't make sense if `f = (Maybe m)` because
then you end up with `Maybe m a` (invalid as `Maybe` takes one type parameter).

## Tree as functor

Recall definition of `Tree`

    data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

If we want Tree to be a Functor, we need to define the semantics of mapping
over it. For example, we could decide

 - map over an empty tree -> empty tree
 - map over a non-empty tree -> function applied to root and map over its
   subtrees (i.e. recursion)

To implement this:

    instance Functor Tree where
        fmap f EmptyTree = EmptyTree
        fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

e.g.

    > fmap (+2) EmptyTree
    EmptyTree

    > fmap (+4) foldr treeInsert EmptyTree [5,7,3])
    Node 7 EmptyTree (Node 11 EmptyTree EmptyTree) EmptyTree)

here we're using foldr to construct a tree:

    > foldr treeInsert EmptyTree [5,7,3]
    Node 3 EmptyTree (Node 7 (Node 5 EmptyTree EmptyTree) EmptyTree)

## Either as functor

Functors must be type constructors that take one type parameter. `Either` takes
two. So we make `Either a` a Functor.

Referring to the type of `fmap` in the defintion of Functor, that's going to
require us to implement an `fmap` function of type 

    `(b -> c) -> Either a b -> Either a c`

Here's how that's done in the standard library:

    instance Functor (Either a) where
        fmap (Right x) = Right (f x)
        fmap (Left x) = Left x

The function is mapped in the case of a `Right` value, but it isn't in the case
of the `Left`. That's consistent with the type definition of `fmap`.

In terms of the box analogy, think of Left as an empty box (or an error
message), which is unchanged by the application of the function.

## Map as functor

`Map k v` will map a function of type `v -> v'`. This is perhaps implemented
like this?

    instance Functor (Map k) where
        fmap Map  = Map k (f v)

except not really because it needs to iterate over all the key/item pairs.
Maybe something like this?

    instance Functor (Map k) where
        fmap f m = fromList . map (\(key, val) -> (key, f val)) $ toList m
    
## Kinds

Kinds are the type of a type. Some types are concrete (e.g. `Int`, some are
partially applied, e.g. `Either String`)

    > :k Int
    Int :: *
    > :k Maybe
    Maybe :: * -> *
    > :k Either String
    Either String :: * -> *

Int is a concrete type. (Read * as "type".)

Maybe is a type that takes a type and returns a (concrete) type.

Same for `Either String`.

    > :k Either
    Either :: * -> * -> *

As an example of what this is useful for (behind the scenes), the Functor class
requires types of kind `* -> *`. That's why we needed to partially apply
`Either`.
