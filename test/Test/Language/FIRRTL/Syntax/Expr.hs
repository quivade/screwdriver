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

field :: Orientation -> Ident -> Type -> Field Type
field = Field

exBundle :: Type
exBundle = Fix $ Bundle [ field Direct "a" (Fix $ Ground $ Signed (Just 3))
                        , field Direct "b" (Fix $ Ground $ Unsigned Nothing)
                        , field Flipped "c" (Fix $ Ground $ Unsigned (Just 1))
                        ]

subFieldExpr :: ExprF TypedExpr -> Ident -> TypedExpr
subFieldExpr b id = annotate Nothing b

env = singleton "bundle" (poly exBundle)

ref :: TypedExpr
ref = annotate Nothing (Ref "bundle")

unifyBundle = testCase "unify bundle type" $
  either
    (assertFailure . show)
    (\p -> assertBool "unify bundle type" True)
    (runIdentity
      $ evalIntBindingT
      $ runExceptT
      $ typecheck env
      $ annotate Nothing (SubField ref "a") )

literal = testCase "unify single literal value" $
  either
    (assertFailure . show)
    (\p -> assertBool "typecheck completed" True)
    (runIdentity
      $ evalIntBindingT
      $ runExceptT
      $ typecheck env
      $ constExpr (sint 5) (Just (Fix $ Ground $ Signed (Just 4))))
