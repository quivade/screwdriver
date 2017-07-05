module Language.Verilog.Parser.Expression where

import           Control.Applicative         ((<$>), (<*>), (<|>))
import qualified Data.HashSet                as HashSet
import           Text.Parser.Expression
import           Text.Parser.Token
import           Text.Parser.Token.Highlight
import           Text.Trifecta

import           Language.Verilog.Parser.Number (number)
import           Language.Verilog.Syntax.Number

vlog2005Ops :: TokenParsing m => IdentifierStyle m
vlog2005Ops = IdentifierStyle
  { _styleName                  = "operator"
  , _styleStart                 = _styleLetter vlog2005Ops
  , _styleLetter                = oneOf "+-*/><!&|=~^?"
  , _styleReserved              = HashSet.fromList vlog2005ReservedOps
  , _styleHighlight             = Operator
  , _styleReservedHighlight     = ReservedOperator
  }

expr :: (Monad m, TokenParsing m) => m Number
expr = buildExpressionParser vlog2005OpTable term
     <?> "expression"

term :: (Monad m, TokenParsing m) => m Number
term = parens expr
     <|> number
     <?> "simple expression"

vlog2005ReservedOps :: [String]
vlog2005ReservedOps =
  -- {} {{}} Concatenation, replication
  [ "+", "-" -- Unary operators
  , "+", "-", "*", "/", "**" -- Arithmetic
  , "%" -- Modulus
  , ">", ">=", "<", "<=" -- Relational
  , "!" -- Logical negation
  , "&&" -- Logical and
  , "||" -- Logical or
  , "==" -- Logical equality
  , "!=" -- Logical inequality
  , "===" -- Case equality
  , "!==" -- Case inequality
  , "~" -- Bitwise negation
  , "&" -- Bitwise and
  , "|" -- Bitwise inclusive or
  , "^" -- Bitwise exclusive or
  , "^~", "~^" -- Bitwise equivalence
  , "&" -- Reduction and
  , "~&" -- Reduction nand
  , "|" -- Reduction or
  , "~|" -- Reduction nor
  , "^" -- Reduction xor
  , "~^", "^~" -- Reduction xnor
  , "<<" -- Logical left shift
  , ">>" -- Logical right shift
  , "<<<" -- Arithmetic left shift
  , ">>>" -- Arithmetic right shift
  , "?:" -- Conditional
  ]

vlog2005OpTable :: (Monad m, TokenParsing m) => [[Operator m Number]]
vlog2005OpTable = [ [prefix "-" negate, prefix "+" id]
                  -- , [binary ">" gt, binary ">=" gte, binary "<" lt, binary "<=" lte]
                  ]

binary  name fun = Infix (fun <$ reservedOp name)
prefix  name fun = Prefix (fun <$ reservedOp name)
postfix name fun = Postfix (fun <$ reservedOp name)

reservedOp :: (Monad m, TokenParsing m) => String -> m ()
reservedOp = reserve vlog2005Ops
