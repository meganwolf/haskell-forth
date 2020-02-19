module Main where

-- Running: runaskell Main.hs path_to_test_file

import Interpret
import System.Environment

main :: IO ()
main = do
    (fileName:tl) <- getArgs
    contents <- readFile fileName
    let (stack, output) = interpret contents 
    putStrLn output
    -- check whether the stack is empty, if stack is not empty, tell what the contents of the stack are

    -- Change the code in `Main.hs` so that, if the stack is not empty at the end of execution, 
    -- a message gets printed on the screen saying so and the stack content gets printed. 
    -- Make sure at least one of your test cases covers this situation.
    -- if stack /= [] then putStrLn stack

     
        
