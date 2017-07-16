module Language.FIRRTL.Parser.Common where

import           Control.Applicative
import qualified Data.HashSet                as HashSet
import           Data.Monoid                 ((<>))
import           Text.Parser.Token.Highlight
import           Text.Trifecta

import Language.FIRRTL.Syntax

firrtlIdents :: TokenParsing m => IdentifierStyle m
firrtlIdents = IdentifierStyle
  { _styleName = "identifier"
  , _styleStart = letter <|> char '_'
  , _styleLetter = alphaNum <|> char '_'
  , _styleReserved = firrtlReserved <> firrtlPrimOps
  , _styleHighlight = Identifier
  , _styleReservedHighlight = ReservedIdentifier
  }

firrtlReserved :: HashSet.HashSet String
firrtlReserved = HashSet.fromList
  [ "Analog", "attach"
  , "circuit", "Clock"
  , "data-type", "depth"
  , "else", "extmodule"
  , "Fixed", "flip"
  , "input", "inst", "invalid"
  , "mem", "module", "mux"
  , "new", "node"
  , "old", "output"
  , "printf"
  , "reader", "readwriter", "read-latency", "read-under-write", "reg"
  , "SInt", "skip", "stop"
  , "undefined", "UInt"
  , "when", "wire", "writer", "write-latency"
  , "validif"
  ]

firrtlPrimOps :: HashSet.HashSet String
firrtlPrimOps = HashSet.fromList
  [ "add"
  , "sub"
  , "mul"
  , "div"
  , "mod"
  , "lt"
  , "leq"
  , "gt"
  , "geq"
  , "eq"
  , "neq"
  , "pad"
  , "asUInt"
  , "asSInt"
  , "asClock"
  , "shl"
  , "shr"
  , "dshl"
  , "dshr"
  , "cvt"
  , "neg"
  , "not"
  , "and"
  , "or"
  , "xor"
  , "andr"
  , "orr"
  , "xorr"
  , "cat"
  , "bits"
  , "head"
  , "tail"
  ]

identifier :: (Monad m, TokenParsing m) => m Ident
identifier = ident firrtlIdents

reserved :: (Monad m, TokenParsing m) => String -> m ()
reserved = reserve firrtlIdents
