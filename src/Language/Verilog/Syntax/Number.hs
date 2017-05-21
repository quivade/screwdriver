module Language.Verilog.Syntax.Number
  ( Sign (..)
  , Base (..)
  , Number (..)
  ) where

import Data.Bits
import Language.Verilog.Syntax.Number.Value

data Sign = Unsigned | Signed
  deriving Eq

instance Show Sign where
  show Unsigned = ""
  show Signed   = "s"

data Number = Number
  { numberSign  :: Sign
  , numberSize  :: Integer
  , numberValue :: [Value]
  } deriving Eq

instance Show Number where
  show n = '\'' : show (numberSign n) ++ show (numberValue n)

zipWith' :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWith' _ []     []     = []
zipWith' f (a:as) []     = a : zipWith' f as []
zipWith' f []     (b:bs) = b : zipWith' f [] bs
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs

maxSize :: Number -> Number -> Integer
maxSize a b = max (numberSize a) (numberSize b)

bitwiseBinOp :: (Value -> Value -> Value) -> Number -> Number -> Number
bitwiseBinOp f n1 n2 = Number Unsigned (maxSize n1 n2)
  (zipWith' f (numberValue n1) (numberValue n2))

bitwiseUnOp :: (Value -> Value) -> Number -> Number
bitwiseUnOp f n = Number Unsigned (numberSize n) (f <$> numberValue n)

instance Bits Number where
  (.&.) = bitwiseBinOp (.&.)

  (.|.) = bitwiseBinOp (.|.)
  xor = bitwiseBinOp xor

  complement = bitwiseUnOp complement

data Base
  = HexBase
  | DecBase
  | OctBase
  | BinBase

instance Show Base where
  show HexBase = "h"
  show DecBase = "d"
  show OctBase = "o"
  show BinBase = "b"
