module Language.Verilog.Syntax.Number
  ( module Language.Verilog.Syntax.Number.Value
  , Sign (..)
  , Number (..)
  , carrySum
  , decValue
  , toInt
  , signExtend
  , shiftExtendL
  ) where

import qualified Prelude as P
import           Prelude hiding (take)

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

bitwiseBinOp :: (Value -> Value -> Value) -> Number -> Number -> Number
bitwiseBinOp f n1 n2 = Number Unsigned (maxSize n1 n2)
  (zipWith' f (numberValue n1) (numberValue n2))
    where maxSize :: Number -> Number -> Int
          maxSize a b = max (numberSize a) (numberSize b)

bitwiseUnOp :: (Value -> Value) -> Number -> Number
bitwiseUnOp f (Number sign size val) = Number sign size (f <$> val)

instance Bits Number where
  (.&.) = bitwiseBinOp (.&.)

  (.|.) = bitwiseBinOp (.|.)
  xor = bitwiseBinOp xor

  complement = bitwiseUnOp complement

  -- both << (logical) and <<< (arithmetic)
  shiftL num s = take (numberSize num) (shiftExtendL num s)

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

wider :: Number -> Number -> (Number, Number)
wider n1 n2 =
  if numberSize n1 < numberSize n2
     then (n2, n1)
     else (n1, n2)

zeroPad :: Number -> Int -> Number
zeroPad (Number sign s v) n = Number sign n
  $ v ++ replicate (n - s) Zero

signExtend :: Number -> Int -> Number
signExtend (Number Signed s v) n = Number Signed n
  $ v ++ replicate (n - s) (last v)
signExtend n u = zeroPad n u

carrySum :: Value -> [Value] -> [Value] -> [Value]
carrySum _ []     []     = []
carrySum c []     ys     = carrySum c ys []
carrySum c (x:xs) []     = let (out, carry) = fullAdder c x Zero
                            in out:carrySum carry xs []
  -- let (out, carry) = fullAdder c Zero y
  --                           in out:carrySum carry [] ys
carrySum c (x:xs) (y:ys) = let (out, carry) = fullAdder c x y
                            in out:carrySum carry xs ys

carryAdd :: Value -> Number -> Number -> [Value]
carryAdd c x y = let size = max (numberSize x) (numberSize y)
                  in carrySum c (numberValue $ signExtend x (size + 1))
                  (numberValue $ signExtend y (size + 1))

toSigned :: Number -> Number
toSigned (Number Unsigned size val) = Number Signed size val
toSigned n = n

-- | logical shift left with size extension
shiftExtendL :: Number -> Int -> Number
shiftExtendL (Number sign size val) s = Number sign (size + s) $ fill ++ val
  where fill = replicate s Zero

take :: Int -> Number -> Number
take n a@(Number sign size val)
  | n <= 0 = a
  | n > size = signExtend a n
  | otherwise = Number sign n $ P.take n val

asSigned :: Number -> Number
asSigned (Number _ size val) = Number Signed size val

asUnsigned :: Number -> Number
asUnsigned (Number _ size val) = Number Unsigned size val

instance Num Number where
  x@(Number Signed sx _) + y@(Number Signed sy _)
    | sx == sy = Number Signed sx
      $ carrySum Zero
        (numberValue $ signExtend x sx)
        (numberValue $ signExtend y sx)
    | otherwise =
      let size = max sx sy
       in signExtend x size + signExtend y size

  x@(Number Unsigned sx _) + y@(Number Unsigned sy _)
    | sx == sy = Number Unsigned sx
      $ carrySum Zero
        (numberValue $ signExtend x sx)
        (numberValue $ signExtend y sx)
    | otherwise =
      let size = max sx sy
       in signExtend x size + signExtend y size

  x + y = asUnsigned x + asUnsigned y

  x@(Number Unsigned sx vx) * y@(Number Unsigned sy vy) =
    let size = max sx sy
        double = 2 * size
        p = signExtend x double
        q = signExtend y double

        multiply :: Number -> Int -> [Value] -> [Value] -> Number
        multiply acc _ _ [] = acc
        multiply acc n a (b:bs) =
          multiply (acc + partial n a b) (n + 1) a bs

        partial :: Int -> [Value] -> Value -> Number
        partial n a b = take size
          (shiftExtendL (Number Unsigned (length a) ((*b) <$> a)) n)

     in take size
          (multiply (Number Unsigned 1 [Zero]) 0 (numberValue p) (numberValue q))

  x@(Number Signed sx vx) * y@(Number Signed sy vy) =
    let size = max sx sy
        double = 2 * size
        p = signExtend x double
        q = signExtend y double

        multiply :: Number -> Int -> [Value] -> [Value] -> Number
        multiply acc _ _ [] = acc
        multiply acc n a [b] = acc - partial n a b
        multiply acc n a (b:bs) =
          multiply (acc + partial n a b) (n + 1) a bs

        partial :: Int -> [Value] -> Value -> Number
        partial n a b = take size
          (shiftExtendL (Number Signed (length a) ((*b) <$> a)) n)

     in take size (multiply (fromInteger 0) 0 (numberValue p) (numberValue q))

  x * y = asUnsigned x * asUnsigned y

  negate num =
    let size = numberSize num
     in toSigned (complement num)
       + Number Signed size (One:replicate (size - 1) Zero)

  abs num@(Number Signed size val)
    | last val == One = negate num
    | otherwise = num
  abs num = num

  signum num@(Number Signed size val)
    | int < 0 = Number Signed size $ replicate size One
    | int > 0 = Number Signed size (One : replicate (size - 1) Zero)
    | otherwise = Number Signed size $ replicate size Zero
    where int = toInt num

  fromInteger n =
    let dec = decValue (abs n)
        val
          | n < 0 && abs n == 2 ^ (length dec - 1) = dec
          | n == 0 = dec
          | otherwise = dec ++ [Zero]
        size = length val
        neg = if n < 0 then negate
                       else id
     in neg $ Number Signed size val

-- | Sort of like Bounded class but depends on runtime size value
minNumber, maxNumber :: Number -> Number
minNumber (Number Unsigned s _) = Number Unsigned s $ replicate s Zero
minNumber (Number Signed   s _) = Number Signed   s $ One : replicate (s-1) Zero
maxNumber (Number Unsigned s _) = Number Unsigned s $ replicate s One
maxNumber (Number Signed   s _) = Number Signed   s $ Zero : replicate (s-1) One

decValue :: Integer -> [Value]
decValue 0 = [Zero]
decValue 1 = [One]
decValue i = decValue (i `rem` 2) ++ decValue (i `quot` 2)

toInt :: Number -> Int
toInt (Number Unsigned s val) = go 0 val
  where go _ [] = 0
        go n (One:vs) =  2 ^ n + go (n + 1) vs
        go n (Zero:vs) = go (n + 1) vs
toInt n@(Number Signed s val) =
  if last val == One
     then negate $ toInt (Number Unsigned s (numberValue $ negate n))
     else toInt $ Number Unsigned s val

-- | Verilog comparisons which return:
-- 1'b0 for false
-- 1'b1 for true
-- 1'bx if any of the compared numbers contain X or Z
-- lt, gt, eq :: Number -> Number -> Number
-- lt n1@(Number Signed s1 v1) n2@(Number Signed s2 v2)
--   | any (\e -> e == Unknown || e == HighZ) v1 = unknown
--   | any (\e -> e == Unknown || e == HighZ) v2 = unknown
--   | toInt n1 < toInt n2 = one
--   | otherwise = zero

-- gt n1@(Number Signed s1 v1) n2@(Number Signed s2 v2)
--   | any (\e -> e == Unknown || e == HighZ) v1 = unknown
--   | any (\e -> e == Unknown || e == HighZ) v2 = unknown
--   | toInt n1 > toInt n2 = one
--   | otherwise = zero

-- eq n1@(Number Signed s1 v1) n2@(Number Signed s2 v2)
--   | any (\e -> e == Unknown || e == HighZ) v1 = unknown
--   | any (\e -> e == Unknown || e == HighZ) v2 = unknown
--   | toInt n1 == toInt n2 = one
--   | otherwise = zero
