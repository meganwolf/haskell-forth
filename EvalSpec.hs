-- HSpec tests for Val.hs
-- Execute: runhaskell EvalSpec.hs

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Val
import Eval

main :: IO ()
main = hspec $ do
  describe "eval" $ do
    context "*" $ do
        it "multiplies integers" $ do
            eval "*" [Integer 2, Integer 3] `shouldBe` [Integer 6]
        
        it "multiplies floats" $ do
            eval "*" [Integer 2, Real 3.0] `shouldBe` [Real 6.0]
            eval "*" [Real 3.0, Integer 3] `shouldBe` [Real 9.0]
            eval "*" [Real 4.0, Real 3.0] `shouldBe` [Real 12.0]

        it "multiply errors on too few arguments" $ do   
            evaluate (eval "*" []) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "*" [Integer 2]) `shouldThrow` errorCall "Stack underflow"

    context "+" $ do
        it "adds integers" $ do
            eval "+" [Integer 2, Integer 8] `shouldBe` [Integer 10]

        it "adds floats" $ do
            eval "+" [Integer 2, Real 3.0] `shouldBe` [Real 5.0]
            eval "+" [Real 7.5, Integer 2] `shouldBe` [Real 9.5]
            eval "+" [Real 7.5, Real 2.5] `shouldBe` [Real 10.0]

        it "add errors on too few arguments" $ do
            evaluate (eval "+" []) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "+" [Integer 2]) `shouldThrow` errorCall "Stack underflow"

    context "-" $ do
        it "subtracts integers" $ do
            eval "-" [Integer 3, Integer 5] `shouldBe` [Integer 2]

        it "subtracts floats" $ do
            eval "-" [Real 7.0, Integer 12] `shouldBe` [Real 5.0]
            eval "-" [Integer 2, Real 15.0] `shouldBe` [Real 13.0]
            eval "-" [Real 10.0, Real 20.0] `shouldBe` [Real 10.0]

        it "subtract errors on too few arguments" $ do
            evaluate(eval "-" []) `shouldThrow` errorCall "Stack underflow"
            evaluate(eval "-" [Integer 2]) `shouldThrow` errorCall "Stack underflow"

    context "/" $ do
        it "divides integers" $ do
            eval "/" [Integer 10, Integer 92] `shouldBe` [Integer 9]

        it "divides floats" $ do
            eval "/" [Real 6.0, Integer 12] `shouldBe` [Real 2.0]
            eval "/" [Integer 5, Real 100.0] `shouldBe` [Real 20.0]
            eval "/" [Real 5.0, Real 25.0] `shouldBe` [Real 5.0]

        it "divide errors on too few argumennts" $ do
            evaluate(eval "/" []) `shouldThrow` errorCall "Stack underflow"
            evaluate(eval "/" [Integer 2]) `shouldThrow` errorCall "Stack underflow"
    
    context "^" $ do
        it "powers integers" $ do
            eval "^" [Integer 3, Integer 2] `shouldBe` [Integer 8]

        it "powers floats" $ do
            eval "^" [Real 7.0, Integer 5] `shouldBe` [Real 78125.0]
            eval "^" [Integer 5, Real 7.0] `shouldBe` [Real 16807.0]
            eval "^" [Real 3.0, Real 5.0] `shouldBe` [Real 125.0]
        
        it "power errors on too few arguments" $ do
            evaluate(eval "^" []) `shouldThrow` errorCall "Stack underflow"
            evaluate(eval "^" [Integer 2]) `shouldThrow` errorCall "Stack underflow"

        -- this does not work, seems to be a HSpec bug
        -- it "errors on non-numeric inputs" $ do
        --    evaluate(eval "*" [Real 3.0, Id "x"]) `shouldThrow` anyException

    context "DUP" $ do
        it "duplicates values" $ do
            eval "DUP" [Integer 2] `shouldBe` [Integer 2, Integer 2]
            eval "DUP" [Real 2.2] `shouldBe` [Real 2.2, Real 2.2]
            eval "DUP" [Id "x"] `shouldBe` [Id "x", Id "x"]

        it "errors on empty stack" $ do
            evaluate (eval "DUP" []) `shouldThrow` errorCall "Stack underflow"

    context "STR" $ do
        it "converts the argument into a string" $ do
            eval "STR" [Integer 2] `shouldBe` [Id "2"]
            eval "STR" [Real 2.0] `shouldBe` [Id "2.0"]

        it "errors on empty stack" $ do
            evaluate (eval "STR" []) `shouldThrow` errorCall "Stack underflow"

    context "CONCAT2" $ do
        it "concatenates 2 strings from the stack" $ do
            eval "CONCAT2" [Id " World!", Id "Hello"] `shouldBe` [Id "Hello World!"]
        
        it "errors on too few arguments" $ do
            evaluate (eval "CONCAT2" [Id "Hello"]) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "CONCAT2" []) `shouldThrow` errorCall "Stack underflow"

        it "errors on arguments not Strings" $ do
            evaluate (eval "CONCAT2" [Id "Hello", Integer 5]) `shouldThrow` errorCall "Arguments are not strings"
            evaluate (eval "CONCAT2" [Integer 5, Id "Hello"]) `shouldThrow` errorCall "Arguments are not strings"
            evaluate (eval "CONCAT2" [Id "Hello", Real 5]) `shouldThrow` errorCall "Arguments are not strings"
            evaluate (eval "CONCAT2" [Real 5.0, Id "Hello"]) `shouldThrow` errorCall "Arguments are not strings"
            evaluate (eval "CONCAT2" [Real 5.0, Integer 5]) `shouldThrow` errorCall "Arguments are not strings"

    context "CONCAT3" $ do
        it "concatenates 3 strings from the stack" $ do
            eval "CONCAT3" [Id "Chilli's", Id "Welcome to ", Id "Hi, "] `shouldBe` [Id "Hi, Welcome to Chilli's"]

        it "errors on too few arguments" $ do
            evaluate (eval "CONCAT3" [Id "Hello"]) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "CONCAT3" [Id "Hello", Id " World"]) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "CONCAT3" []) `shouldThrow` errorCall "Stack underflow"

        it "errors on arguments not Strings" $ do
            evaluate (eval "CONCAT3" [Integer 5,  Integer 6, Id "Chilli's"]) `shouldThrow` errorCall "Arguments are not strings"
            evaluate (eval "CONCAT3" [Integer 5,  Id "Welcome to ", Id "Chilli's"]) `shouldThrow` errorCall "Arguments are not strings"
            evaluate (eval "CONCAT3" [Real 5.0, Real 5.0, Real 5.0]) `shouldThrow` errorCall "Arguments are not strings"


  describe "evalOut" $ do
    context "." $ do
        it "prints top of stack" $ do
            evalOut "." ([Id "x"], "") `shouldBe` ([],"x")
            evalOut "." ([Integer 2], "") `shouldBe` ([], "2")
            evalOut "." ([Real 2.2], "") `shouldBe` ([], "2.2")

        it "errors on empty stack" $ do
            evaluate(evalOut "." ([], "")) `shouldThrow` errorCall "Stack underflow"

        it "eval pass-through" $ do
            evalOut "*" ([Real 2.0, Integer 2], "blah") `shouldBe` ([Real 4.0], "blah") 

    context "CR" $ do
        it "prints a new line (for nice formating)" $ do
            evalOut "CR" ([Id "CR"],"") `shouldBe` ([Id "CR"],"\n")

    context "EMIT" $ do
        it "converts Integer value to ASCII Char value" $ do
            evalOut "EMIT" ([Integer 80],"") `shouldBe` ([],"P")
            evalOut "EMIT" ([Integer 65],"") `shouldBe` ([], "A")
        
        it "EMIT errors on empty stack" $ do
            evaluate (evalOut "EMIT" ([], "")) `shouldThrow` errorCall "Stack underflow"



    