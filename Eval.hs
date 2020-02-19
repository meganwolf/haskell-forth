module Eval where
-- This file contains definitions for functions and operators

import Val

-- main evaluation function for operators and 
-- built-in FORTH functions with no output
-- takes a string and a stack and returns the stack
-- resulting from evaluation of the function
eval :: String -> [Val] -> [Val]
-- Multiplication
-- if arguments are integers, keep result as integer
eval "*" (Integer x: Integer y:tl) = Integer (x*y) : tl
-- if any argument is float, make result a float
eval "*" (x:y:tl) = (Real $ toFloat x * toFloat y) : tl 
-- any remaining cases are stacks too short
eval "*" _ = error("Stack underflow")

-- Write added code here

-- Addition
-- if arguments are integers, keep result as integer
eval "+" (Integer x: Integer y:tl) = Integer (x+y) : tl
-- if any argument is a float, make result a float
eval "+" (x:y:tl) = (Real $ toFloat x + toFloat y) : tl
-- any remaining cases are stacks too short
eval "+" _ = error("Stack underflow")

-- Subtraction
-- if arguments are integers, keep result as integer
eval "-" (Integer x: Integer y:tl) = Integer (x-y) : tl
-- if any argument is a float, make result a float
eval "-" (x:y:tl) = (Real $ toFloat x - toFloat y) : tl
-- any remaining cases are stacks too short
eval "-" _ = error("Stack underflow")

-- Division
-- if arguments are integers, keep result as integer
eval "/" (Integer x: Integer y:tl) = Integer (div x y) : tl
-- if any argument is a float, make result a float
eval "/" (x:y:tl) = (Real $ toFloat x / toFloat y) : tl
-- any remaining cases are stacks too short
eval "/" _ = error("Stack underflow")

-- Power
-- if arguments are integers, keep result as integer
eval "^" (Integer x: Integer y:tl) = Integer (x^y) : tl
-- if any argument is a float, make result a float
eval "^" (x:y:tl) = (Real $ toFloat x ** toFloat y) : tl
-- any remaining cases are stacks too short
eval "^" _ = error("Stack underflow")
  


-- Duplicate the element at the top of the stack
eval "DUP" (x:tl) = (x:x:tl)
eval "DUP" [] = error("Stack underflow")

-- this must be the last rule
-- it assumes that no match is made and preserves the string as argument
eval s l = Id s : l 


-- variant of eval with output
-- state is a stack and string pair
evalOut :: String -> ([Val], String) -> ([Val], String) 
-- print element at the top of the stack
evalOut "." (Id x:tl, out) = (tl, out ++ x)
evalOut "." (Integer i:tl, out) = (tl, out ++ (show i))
evalOut "." (Real x:tl, out) = (tl, out ++ (show x))
evalOut "." ([], _) = error "Stack underflow"

-- `EMIT`: takes a number from the stack and prints the character with the corresponding ASCI code
evalOut "EMIT" (Integer i:tl, out) = (tl, out ++ [toEnum i])
evalOut "EMIT" ([], _) = error "Stack underflow"

-- `CR`: prints a new line (for nice formating)

-- `STR`: converts the argument into a string (needs to work for all types)

-- `CONCAT2` and `CONCAT3` concatenates 2 or 3 strings from the stack (errors if arguments not strings)


-- this has to be the last case
-- if no special case, ask eval to deal with it and propagate output
evalOut op (stack, out) = (eval op stack, out)