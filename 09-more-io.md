# Chapter 9: More input and output

## Input redirection

The `getLine` action can get from files using standard unix redirection, e.g.

    $ cat name.txt
    Mike
    $ ./sayname < name.txt
    Hello Mike

## Getting strings from streams

    import Data.Char

    main = do
        contents <- getContents
        putStr $ map toUpper contents

getContents lazily gets a file. It won’t try to read all of the content at once
and store that into memory before printing out the caps-locked version. Rather,
it will print out the caps-locked version as it reads, because it will read a
line from the input only when it must.

This program prints out input that is shorter than ten characters

    main = do
        contents <- getContents
        putStr (shortLinesOnly contents)

    shortLinesOnly :: String -> String
    shortLinesOnly = unlines . filter (\line -> length line < 10) . lines

`lines` splits strings on `\n`. `unlines` does the opposite. Note the IO bit
(the do block) is made shorter by moving stuff into functions.

In fact it can be even shorter using `interact`, which is basically "get input,
apply function and echo result". With that, `main` becomes

    main = interact shortLinesOnly

This program prints whether each line of input is a palindrome

    respondPalindromes :: String -> String
    respondPalindromes =
        unlines .
        map (\xs -> if isPal xs then "palindrome" else "not a palindrome") .
        lines

    isPal :: String -> Bool
    isPal xs = xs == reverse xs

    main = interact respondPalindromes

It does this lazily: it doesn't wait until it has all the input lines (i.e.
until Ctrl-D) to start printing output.

## File input and output

### Vanilla

    $ cat girlfriend.txt
    Hey! Hey! You! You!
    I don't like your girlfriend!
    No way! No way!
    I think you need a new one!

and

    $ cat girlfriend.hs
    import System.IO

    main = do
        handle <- openFile "girlfriend.txt" ReadMode
        contents <- hGetContents handle
        putStr contents
        hClose handle

then

    $ ./girlfriend
    Hey! Hey! You! You!
    I don't like your girlfriend!
    No way! No way!
    I think you need a new one!

The new functions (actually IO actions) have these types

    openFile :: FilePath -> IOMode -> IO Handle
    hGetContents :: Handle -> IO String
    hClose :: Handle -> IO ()

and IOMode is a type that's defined like this

    data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
    
This type is an enumeration that represents what we want to do with our opened
file.

### Using `withFile`

    import System.IO

    main = do
        withFile "girlfriend.txt" ReadMode (\handle -> do
            contents <- hGetContents handle
            putStr contents)

where

    withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a

`(\handle -> ...)` is the function that takes a handle and returns an I/O
action, and it’s usually done like this, with a lambda

This is like context manager `with open("file.txt") as f:` in python in that it
automatically closes the file, even if an exception is raised.

## `bracket`

This seems like `__exit__` or `finally` in python, i.e. to ensure code gets run
even if there's an exception.

    bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c

Its first parameter is an I/O action that acquires a resource, such as a file
handle. Its second parameter is a function that releases that resource. This
function gets called even if an exception has been raised. The third parameter
is a function that also takes that resource and does something with it. The
third parameter is where the main stuff happens, like reading from a file or
writing to it.

So we could implement `withFile` like this

    withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
    withFile name mode f = bracket (openFile name mode)
        (\handle -> hClose handle)
        (\handle -> f handle)


## Handle functions

`girlfriend.hs` can be simplified by using `readFile`:

    import System.IO

    main = do
        contents <- readFile "girlfriend.txt"
        putStr contents

This code doesn't explicitly assign a handle to a name, so we can't close it.
`readFile` does that for us. There are equivalent `writeFile` and `appendFile`
functions.

    readFile :: FilePath -> IO String
    writeFile :: FilePath -> String -> IO ()
    appendFile :: FilePath -> String -> IO ()

`FilePath` is essentially synonymous with `String`.

## Todo list app

appendtodo.hs

    import System.IO

    main = do
        todoItem <- getLine
        appendFile "todo.txt" (todoItem ++ "\n")

Deleting items is trickier

```
import System.IO
import System.Directory
import Data.List

main = do
    contents <- readFile "todo.txt"
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks

    putStrLn "These are your TO-DO items:"
    mapM_ putStrLn numberedTasks
    putStrLn "Which one do you want to delete?"
    numberString <- getLine
    let number = read numberString
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks

    (tempName, tempHandle) <- openTempFile "." "temp"
    hPutStr tempHandle newTodoItems
    hClose tempHandle
    removeFile "todo.txt"
    renameFile tempName "todo.txt"
```

The indentation after each `let` is significant.

This line

    numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks

maps `["Feed cat", "Walk dog"]` to `["0 - Feed cat", "1 - Walk dog"]`

`numberString` is a string, `number = read numberString` is an `Int`.

Recall `mylist !! x` extracts the xth element from `mylist`. `newTodoItems` is
therefore the original list with the xth item removed.

The rest is pretty obvious. It writes to a temporary file before clobbering the
old todo.txt.

### Cleanup

However, the temporary file doesn't get cleaned up if something goes wrong
after we open it but before we rename it.

We'll use `bracketOnError`. `bracket` is like `__exit__` or `finally`.
`bracketOnError` is more like regular `except`, i.e. it is run if there's an
exception.

So we'll change

    (tempName, tempHandle) <- openTempFile "." "temp"
    hPutStr tempHandle newTodoItems
    hClose tempHandle
    removeFile "todo.txt"
    renameFile tempName "todo.txt"

to

    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoItems
            hClose tempHandle
            removeFile "todo.txt"
            renameFile tempName "todo.txt")

bracketOnError takes an action item, a function that takes the action item and
cleans up if an an exception occurs results in an exception, and a function
that does the normal thing to the action item. It's a bit like:

    except:
        cleanup
    try:
        a thing

in that the except block (the function that takes the action item) comes before
the try block.

## Command line arguments

Use the `getProgName` to get `sys.argv[0]`. Use `getArgs` to get
`sys.argv[1:]`. Both in `System.Environment`, e.g.

    import System.Environment
    import Data.List

    main = do
       args <- getArgs
       progName <- getProgName
       putStrLn "The arguments are:"
       mapM putStrLn args
       putStrLn "The program name is:"
       putStrLn progName

## To-do with command line arguments

We want

    $ ./todo add todo.txt "Find the magic sword of power"
    $ ./todo view todo.txt
    $ ./todo remove todo.txt 2

Here's the program (with notes)

```
import System.Environment
import System.Directory
import System.IO
import Data.List
```

This function takes a string (a command like "add") and returns a function that
takes a list of arguments and returns an IO action (one of `add`, `view` or
`remove` (all of which are defined elsewhere).

```
dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove
```

The main program:

```
main = do
    (command:argList) <- getArgs
    dispatch command argList
```

The `(command:argList) <- getArgs` syntax puts the first argument in `command`
and the remainder in `argList`.

The `add` function is obvious:

```
add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
```

`view` receives a list of arguments, but that list should be of length one, so
we pattern match out the first as `fileName` then print the file with prepended
numbers as before:

```
view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line)
                        [0..] todoTasks
    putStr $ unlines numberedTasks
```

The remove function works as before (except with pattern matching on the
arguments rather than prompting the user for input).

```
remove :: [String] -> IO ()
remove [fileName, numberString] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        number = read numberString
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoItems
            hClose tempHandle
            removeFile fileName
            renameFile tempName fileName)
```

End program.

Adding a `bump` dispatch to move an item to the start of the list looks a lot
like `remove`:

    bump :: [String] -> IO ()
    bump [fileName, numberString] = do
        contents <- readFile fileName
        let todoTasks = lines contents
            number = read numberString
            bumpedItem = todoTasks !! number
            newTodoItems = unlines $ bumpedItem : delete (todoTasks !! number) todoTasks

        bracketOnError (openTempFile "." "temp")
            (\(tempName, tempHandle) -> do
                hClose tempHandle
                removeFile tempName)
            (\(tempName, tempHandle) -> do
                hPutStr tempHandle newTodoItems
                hClose tempHandle
                removeFile fileName
                renameFile tempName fileName)

These lines

    bumpedItem = todoTasks !! number
    newTodoItems = unlines $ bumpedItem : delete (todoTasks !! number) todoTasks

Extract out the bumped item, delete it from the list, prepend it to the list,
then reassemble the string.

### Handling bad input

e.g.

    add :: [String] -> IO ()
    add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
    add _ = putStrLn "The add command takes exactly two arguments"

## Randomness

This section uses `System.Random` (e.g. `+m System.Random`) which was removed
from the Haskell standard library some time since the book was published.
`stack` is a tool for installing an isolated GHC and individual isolated
projects with their own dependencies. You can get `System.Random` by doing, e.g.

    $ brew install haskell-stack
    $ stack new my-project
    $ cd my-project
    $ vim package.yaml
        # add `random` to dependencies
    $ stack build
    $ stack ghci
    > `+m System.Random`

`random` returns a random value of type `a`

    random :: (RandomGen g, Random a) => g -> (a, g)

It takes a `RandomGen` and returns the random value and a new `RandomGen` that
can be used in a next call.

`mkStdGen` makes one of these `RandomGen` variables. It takes an integer (the
seed).

    > random (mkStdGen 100) :: (Int, StdGen)
    (-1352021624,651872571 1655838864)

Here `-1352021624` is the random value, and the second return thing is a
textual representation of the random generator. Note we needed to specify the
type we wanted back with `:: (Int, StdGen)` in this case.

So we can do this:

    threeCoins :: StdGen -> (Bool, Bool, Bool)
    threeCoins gen =
        let (firstCoin, newGen) = random gen
            (secondCoin, newGen') = random newGen
            (thirdCoin, newGen'') = random newGen'
        in  (firstCoin, secondCoin, thirdCoin)

Note in this case Haskell could infer the required type of of the first value
returned by the `random` calls (`Bool`) from context. 

`randoms` takes a `RandomGen` and returns an infinite sequence of random
values, e.g.

    > take 5 $ randoms (mkStdGen 11) :: [Int]
    [-1807975507,545074951,-1015194702,-1622477312,-502893664]

It's implemented something like this (recursively):

    randoms' :: (RandomGen g, Random a) => g -> [a]
    randoms' gen = let (value, newGen) = random gen in value:randoms' newGen

`randomR` takes a range and a generator and produces values in that range.
`randomRs` produces a stream of such values.

    > randomR (1,3) (mkStdGen 100)
    (3,4041414 40692)
    > randomR (1.5,3) (mkStdGen 100)
    (2.3613082150951104,693699796 2103410263)
    > take 10 $ randomRs ('a','z') (mkStdGen 100)
