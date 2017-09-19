{-|
Copyright   : (c) Quivade, 2017
License     : GPL-3
Maintainer  : Jakub Kopański <jakub@quivade.com>
Stability   : experimental
Portability : POSIX

This module provides FIRRTL Expr AST definitions.
|-}
module Language.FIRRTL.Syntax.Expr
  ( Expr
  , ExprF
  ) where

import Language.FIRRTL.Syntax.Annotation
import Language.FIRRTL.Syntax.Common
import Language.FIRRTL.Syntax.Recursion
import Language.FIRRTL.Syntax.Types

data UnaryOp
  = AndR | AsClock | AsSigned | AsUnsigned
  | Cvt | Neg | Not | OrR | Xor | XorR
  deriving Eq

data BinaryOp
  -- | op expr expr
  = Add | And | Cat | Div | DShl | DShr | Eq | Geq | Gt
    | Leq | Lt | Mod | Mul | Neq | Or | Sub | Valid
  -- | op expr int
  | Head | Pad | Shl | Shr | Tail
  deriving Eq

data TernaryOp = Bits | Mux
  deriving Eq

-- | Firrtl literal value
data Literal
  = Nat Int  -- ^ natural integer
  | UInt Int -- ^ unsigned integer
  | SInt Int -- ^ signed integer
  deriving Eq

data ExprF r
  = Lit Literal -- ^ Ground Type or natural integer
  | Var Ident   -- ^ introduce new name
  | Ref Ident   -- ^ reference to a node/var
  -- moved to appropriate arity operations
  -- | Valid r r
  -- | Mux r r r
  -- these are much more specific than mux and valid
  | SubField r Ident
  | SubAccess r r
  | Unary UnaryOp r
  | Binary BinaryOp r r
  | Ternary TernaryOp r r r
  deriving Eq

instance Functor ExprF where
  fmap f (SubField e i) = SubField (f e) i
  fmap f (Unary op e) = Unary op (f e)
  fmap f (Binary op a b) = Binary op (f a) (f b)
  fmap f (Ternary op a b c) = Ternary op (f a) (f b) (f c)
  fmap _ (Lit l) = Lit l
  fmap _ (Ref i) = Ref i
  fmap _ (Var i) = Var i

instance Foldable ExprF where
  foldMap f (Lit l) = mempty
  foldMap f (Ref id) = mempty
  foldMap f (SubField p id) = f p `mappend` foldMap f (Ref id)
  foldMap f (SubAccess p q) = f p `mappend` f q
  foldMap f (Unary _ p) = f p
  foldMap f (Binary _ p q) = f p `mappend` f q
  foldMap f (Ternary _ p q r) = f p `mappend` f q `mappend` f r

instance Traversable ExprF where
  traverse _ (Lit l) = pure (Lit l)
  traverse _ (Ref id) = pure (Ref id)
  traverse f (SubField p id) = SubField <$> f p <*> pure id
  traverse f (SubAccess p q) = SubAccess <$> f p <*> f q
  traverse f (Unary op p) = Unary op <$> f p
  traverse f (Binary op p q) = Binary op <$> f p <*> f q
  traverse f (Ternary op p q r) = Ternary op <$> f p <*> f q <*> f r

type Expr = AnnFix Type ExprF
