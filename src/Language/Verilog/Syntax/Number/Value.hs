module Language.Verilog.Syntax.Number.Value
  ( Value (..) ) where

import Data.Bits

data Value
  = Zero
  | One
  | Unknown
  | HighZ
  deriving (Eq, Ord)

instance Show Value where
  show One     = "1"
  show Zero    = "0"
  show Unknown = "x"
  show HighZ   = "z"
  showList vs s = foldl (\s v -> s ++ show v) s vs
  -- showList [] s = s
  -- showList (v:vs) s = showList vs (s ++ show v)

-- instance Show [Value] where
--   show []     = ""
--   show (v:vs) = show v ++ show vs

instance Num Value where
  Zero + Zero = Zero
  Zero + One  = One
  One  + Zero = One
  One  + One  = Zero
  _    + _    = Unknown

  Zero - Zero = Zero
  Zero - One  = One
  One  - Zero = One
  One  - One  = Zero
  _    - _    = Unknown

  Zero * Zero = Zero
  Zero * One  = Zero
  One  * Zero = Zero
  One  * One  = One
  _    * _    = Unknown

  abs = id

  signum = id

  negate Zero = One
  negate One  = Zero
  negate _    = Unknown

  fromInteger 0 = Zero
  fromInteger 1 = One
  fromInteger _ = Unknown

instance Bits Value where
  (.&.) = (*) -- bitwise and
  -- bitwise or
  Zero .|. Zero = Zero
  Zero .|. One  = One
  One  .|. Zero = One
  One  .|. One  = One
  _    .|. _    = Unknown


  xor Zero Zero = Zero
  xor Zero One  = One
  xor One  Zero = One
  xor One  One  = Zero
  xor _    _    = Unknown

  complement = negate

  shift x 0 = x
  shift _ _ = Zero

  rotate x _ = x

  bit 0 = One
  bit _ = Zero

  bitSizeMaybe _ = Just 1
  bitSize      _ = 1

  isSigned _ = False

  testBit One 0 = True
  testBit _   _ = False

  popCount One = 1
  popCount _   = 0

  zeroBits = Zero

instance FiniteBits Value where
  finiteBitSize _ = 1

  countTrailingZeros Zero = 1
  countTrailingZeros _    = 0

  countLeadingZeros Zero = 1
  countLeadingZeros _    = 0
