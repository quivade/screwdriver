{- this is sort of like FIRRTL AST
    but with some restrictions -}
module Language.FIRRTL.Low where

-- | only ground types are used
-- and types have explicit widths
data Type
  = Unsigned Int
  | Signed Int
  | Clock
  -- | Vector Type Int
  -- | Bundle [Field]
  -- return type of statements
  | Unit
  deriving (Eq, Show)

-- | compared to FIRRTL AST there is no
-- conditional & partial connect statement
data Statement
  = Block [Statement]
  -- | Cond Expr Statement Statement
  | Connect Expr Expr
  | Empty
  | Instance Ident Ident
  | Invalid Expr
  | Memory Mem
  | Node Ident Expr
  -- | Partial Expr Expr
  | Print Expr Expr String [Expr]
  | Reg Ident Type (Maybe Expr) (Maybe Reset)
  | Stop Expr Expr Int
  | Wire Ident Type
  deriving (Eq, Show)
