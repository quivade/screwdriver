module Language.Verilog.Syntax.Attribute where

data Attribute = Attribute
  { attributeName :: Text
  -- | TODO: add expressions
  -- , attributeExpr :: Maybe Expr
  } deriving (Eq, Show)
