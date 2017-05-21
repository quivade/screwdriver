module Language.Verilog.Parser.Expression where

import           Control.Applicative         ((<$>), (<*>), (<|>))
import qualified Data.HashSet                as HashSet
import           Text.Parser.Expression
import           Text.Parser.Token
import           Text.Trifecta

vlog2005Ops :: TokenParsing m => IdentifierStyle m
vlog2005Ops = IdentifierStyle
  { _styleName                  = "operator"
  , _styleStart                 = _styleLetter vlog2005Ops
  , _styleLetter                = oneOf "+-*/><!&|=~^?"
  , _styleReserved              = HashSet.fromList vlog2005ReservedOps
  , _styleHighlight             = Operator
  , _styleReservedHighlight     = ReservedOperator
  }

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
vlog2005OpTable == [ [



