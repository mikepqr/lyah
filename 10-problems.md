# Functionally solving problems

## RPN calculator

Consider the input `2 2 +`. An RPN calculator reads from left to right. It puts
numbers onto a stack. When it encounters an operator it pops the appropriate
number of operands off the stack (two in the case of `+`), performs the
operation and puts the result onto the stack. For well-formed input, the stack
should contain a single operand (the value of the input).

In Haskell:

```
solveRPN :: String -> Double
solveRPN = head . foldl foldingFunction [] . words
    where  foldingFunction (x:y:ys) "*" = (y * x):ys
           foldingFunction (x:y:ys) "+" = (y + x):ys
           foldingFunction (x:y:ys) "-" = (y - x):ys
           foldingFunction xs numberString = read numberString:xs
```

`words` tokenizes the input. `foldl` because we are reading left to right. The
stack is initially an empty list. `foldingFunction` pattern matches on
the second argument of the fold (i.e. the input from the string).

If the input is one of the three operators, it performs the calculation on the
first (most recent) two items on the stack, and puts the result back on the
stack (at the front).

If the input is a number that just gets parsed to a number and pushed onto the
stack.

The `head` function ensures that we return, e.g. `42` rather than `[42]`.
