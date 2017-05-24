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
  , numberSize  :: Int
  , numberValue :: [Value]
  } deriving Eq

instance Show Number where
  show n = '\'' : show (numberSign n) ++ show (numberValue n)

zipWith' :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWith' _ []     []     = []
zipWith' f (a:as) []     = a : zipWith' f as []
zipWith' f []     (b:bs) = b : zipWith' f [] bs
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs

maxSize :: Number -> Number -> Int
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

  -- both << (logical) and <<< (arithmetic)
  shiftL (Number sign size val) s = Number sign size $
    take size $ fill ++ val
      where fill = replicate s Zero

  -- >> logical operator
  shiftR (Number sign size val) s = Number sign size $
    drop d new
      where fill = replicate s Zero
            new  = val ++ fill
            d = length new - size

  rotateL n 0 = n
  rotateL (Number sign size (v:vs)) r =
    rotateL (Number sign size $ vs ++ [v]) (r - 1)

  rotateR n 0 = n
  rotateR (Number sign size vs) r =
    rotateR (Number sign size $ last vs : init vs) (r - 1)

  bit n = Number Unsigned n $ reverse $ One : replicate (n - 1) Zero

  bitSizeMaybe (Number _ size _) = Just size
  bitSize = finiteBitSize

  isSigned (Number sign _ _) = case sign of
                                 Signed -> True
                                 Unsigned -> False

  testBit (Number _ size val) n
    | n >= 0 && n <= size = val !! n == One
    | otherwise = False

  popCount (Number _ _ val) = length $ filter (== One) val

  zeroBits = Number Unsigned 1 [Zero]

instance FiniteBits Number where
  finiteBitSize (Number _ size _) = size

  countTrailingZeros (Number _ _ val) =
    length $ takeWhile (== Zero) $ reverse val

  countLeadingZeros (Number _ _ val) =
    length $ takeWhile (== Zero) val

data Base
  = HexBase
  | DecBase
  | OctBase
  | BinBase
  deriving Eq

instance Show Base where
  show HexBase = "h"
  show DecBase = "d"
  show OctBase = "o"
  show BinBase = "b"
