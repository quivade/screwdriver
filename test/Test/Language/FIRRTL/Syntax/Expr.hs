module Test.Language.FIRRTL.Syntax.Expr where

import Control.Unification.IntVar (evalIntBindingT)
import Control.Monad.Trans.Except (runExceptT)
import Data.Functor.Identity      (runIdentity)
import Test.Tasty.HUnit

import Language.FIRRTL.Annotations
import Language.FIRRTL.Recursion
import Language.FIRRTL.Syntax
import Language.FIRRTL.Types
import Language.FIRRTL.Unification

nat :: Int -> ExprF a
nat = Lit . Nat

uint :: Int -> ExprF a
uint = Lit . UInt

sint :: Int -> ExprF a
sint = Lit . SInt

constExpr :: ExprF TypedExpr -> Maybe Type -> TypedExpr
constExpr e mt = annotate mt e


tests = testCase "unify single literal value" $
  either
    (assertFailure . show)
    (\p -> do putStrLn $ show p; assertBool "typecheck completed" True)
    (runIdentity
      $ evalIntBindingT
      $ runExceptT
      $ typecheck
      $ constExpr (sint 5) (Just (Fix $ Ground $ Signed (Just 4))))
