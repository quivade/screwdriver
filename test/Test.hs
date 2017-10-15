module Main where

import Test.Tasty

import qualified Test.Language.FIRRTL.Syntax.Expr as FIR
-- import qualified Test.Language.Verilog.Syntax.Number as Number (properties)
-- import qualified Test.Language.Verilog.Syntax.Number.Value as Value (properties)

main = defaultMain tests

tests :: TestTree
tests = testGroup "screwdriver" [ FIR.literal, FIR.unifyBundle ]
  -- [ Value.properties
  -- , Number.properties
  -- ]
