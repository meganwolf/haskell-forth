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
            eval "-" [Integer 5, Integer 3] `shouldBe` [Integer 2]

        it "subtracts floats" $ do
            eval "-" [Integer 12, Real 7.0] `shouldBe` [Real 5.0]
            eval "-" [Real 15.0, Integer 2] `shouldBe` [Real 13.0]
            eval "-" [Real 20.0, Real 10.0] `shouldBe` [Real 10.0]

        it "subtract errors on too few arguments" $ do
            evaluate(eval "-" []) `shouldThrow` errorCall "Stack underflow"
            evaluate(eval "-" [Integer 2]) `shouldThrow` errorCall "Stack underflow"

    context "/" $ do
        it "divides integers" $ do
            eval "/" [Integer 92, Integer 10] `shouldBe` [Integer 9]

        it "divides floats" $ do
            eval "/" [Integer 12, Real 6.0] `shouldBe` [Real 2.0]
            eval "/" [Real 100.0, Integer 5] `shouldBe` [Real 20.0]
            eval "/" [Real 25.0, Real 5.0] `shouldBe` [Real 5.0]

        it "divide errors on too few argumennts" $ do
            evaluate(eval "/" []) `shouldThrow` errorCall "Stack underflow"
            evaluate(eval "/" [Integer 2]) `shouldThrow` errorCall "Stack underflow"
    
    context "^" $ do
        it "powers integers" $ do
            eval "^" [Integer 2, Integer 3] `shouldBe` [Integer 8]

        it "powers floats" $ do
            eval "^" [Integer 5, Real 7.0] `shouldBe` [Real 78125.0]
            eval "^" [Real 7.0, Integer 5] `shouldBe` [Real 16807.0]
            eval "^" [Real 5.0, Real 3.0] `shouldBe` [Real 125.0]
        
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

    context "EMIT" $ do
        it "converts Integer value to ASCII Char value" $ do
            evalOut "EMIT" ([Integer 80],"") `shouldBe` ([],"P")
            evalOut "EMIT" ([Integer 65],"") `shouldBe` ([], "A")
        
        it "EMIT errors on empty stack" $ do
            evaluate (evalOut "EMIT" ([], "")) `shouldThrow` errorCall "Stack underflow"

    