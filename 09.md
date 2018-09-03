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
