{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Test.Language.Verilog.Syntax.Number.Value where

import Data.Bits
import Test.Invariant
import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC
import Test.QuickCheck (Arbitrary, Gen, arbitrary, choose, frequency, shrink)
import Test.SmallCheck.Series

import Language.Verilog.Syntax.Number
import Language.Verilog.Syntax.Number.Value

import Test.Num (Known (..))
import qualified Test.Num as TN

instance Arbitrary Value where
  arbitrary = do
    n <- frequency [ (5, return 0)
                   , (5, return 1)
                   , (1, return 2)
                   , (1, return 3)
                   ] :: Gen Int
    return $ case n of
      0 -> Zero
      1 -> One
      2 -> Unknown
      3 -> HighZ

  shrink One  = [Zero]
  shrink Zero = []
  shrink HighZ   = [Unknown]
  shrink Unknown = []

instance Monad m => Serial m Value where
  series = cons0 Zero \/ cons0 One \/ cons0 Unknown \/ cons0 HighZ

instance Arbitrary (Known Value) where
  arbitrary = do
    n <- choose (0, 1) :: Gen Int
    return . Known $ case n of
      0 -> Zero
      1 -> One
  shrink = shrink

instance Monad m => Serial m (Known Value) where
  series = Known <$> cons0 Zero \/ cons0 One

properties :: TestTree
properties = testGroup "boolean arithmetic"
  [ secondaryOps, monotone, nonMonotone, hask, num ]

-- | Data.Bits properties
secondaryOps = testGroup "secondary operations"
  [ SC.testProperty "xor" xorOp ]

xorOp :: Value -> Value -> Bool
xorOp x y = (x `xor` y) == ((x .|. y) .&. negate (x .&. y))

-- Value to Bool ?
-- equivalence :: Value -> Value -> Bool
-- equivalence x y = negate xor x y == (x == y)

monotone :: TestTree
monotone = testGroup "monotone laws" [common, boolean]

common = testGroup "common algebra"
  [ SC.testProperty "x ∨ (y ∨ z) = (x ∨ y) ∨ z" $ associativity (.|.)
  , SC.testProperty "x ∧ (y ∧ z) = (x ∧ y) ∧ z" $ associativity (.&.)
  , SC.testProperty "x ∨ y = y ∨ x" $ commutativity (.|.)
  , SC.testProperty "x ∧ y = y ∧ x" $ commutativity (.&.)
  , SC.testProperty "x ∧ (y ∨ z) = (x ∧ y) ∨ (x ∧ z)" $ distribution (.&.) (.|.)
  , SC.testProperty "x ∨ 0 = x" $ identity (.|.) Zero
  , SC.testProperty "x ∧ 0 = x" $ identity (.&.) One
  , SC.testProperty "x ∧ 0 = 0" $ anihilate (.&.) Zero
  ]

boolean = testGroup "boolean algebra specific"
  [ SC.testProperty "x ∨ 1 = 1" $ anihilate (.|.) One
  , SC.testProperty "x ∨ x = x" $ idempotence (.|.)
  , SC.testProperty "x ∧ x = x" $ idempotence (.&.)
  , SC.testProperty "x ∧ (x ∨ y) = x" $ idempotence (.&.)
  , SC.testProperty "x ∨ (x ∧ y) = x" $ idempotence (.&.)
  , SC.testProperty "x ∧ (x ∨ y) = x" $ absorption (.&.) (.|.)
  , SC.testProperty "x ∨ (x ∧ y) = x" $ absorption (.|.) (.&.)
  , SC.testProperty  "x ∨ (y ∧ z) = (x ∨ y) ∧ (x ∨ z)" $ distribution (.|.) (.&.)
  ]

nonMonotone :: TestTree
nonMonotone = testGroup "nonmonotone laws" [complementation, deMorgan]

complementation = testGroup "complelentation"
  [ SC.testProperty "x ∧ ¬x = 0" $ complements (.&.) Zero
  , SC.testProperty "x ∨ ¬x = 1" $ complements (.|.) One
  , SC.testProperty "¬(¬x) = x"  $ doubleNeg complement
  ]

deMorgan = testGroup "De Morgan's laws"
  [ SC.testProperty "¬x ∧ ¬y = ¬(x ∨ y)" $ demorgan (.&.) (.|.) complement
  , SC.testProperty "¬x ∨ ¬y = ¬(x ∧ y)" $ demorgan (.|.) (.&.) complement
  ]

hask = testGroup "Data.Bits properties"
  [ SC.testProperty "clear Zero" clearZero
  , SC.testProperty "set Zero" setZero
  , SC.testProperty "test Zero" testZero
  , SC.testProperty "pop Zero" popZero
  ]
associativity :: (Value -> Value -> Value) -> Value -> Value -> Value -> Bool
associativity = associative

commutativity :: (Value -> Value -> Value) -> Value -> Value -> Bool
commutativity = commutative

distribution :: (Value -> Value -> Value)
             -> (Value -> Value -> Value)
             -> Value -> Value -> Value
             -> Bool
distribution a b = a `distributesOver` b

-- doesn't hold for high impedance
identity :: (Value -> Value -> Value) -> Value -> Known Value -> Bool
identity f a (Known x) = f a x == x

-- doesn't hold for high impedance
-- doesn't hold for unknown
anihilate :: (Value -> Value -> Value) -> Value -> Known Value -> Bool
anihilate f a (Known x) = f a x == a

idempotence :: (Value -> Value -> Value) -> Value -> Bool
idempotence f x = idempotent (f x) x

-- doesn't hold for high impedance
-- doesn't hold for unknown
absorption :: (Value -> Value -> Value)
           -> (Value -> Value -> Value)
           -> Known Value -> Known Value
           -> Bool
absorption f g (Known x) (Known y) = f x (g x y) == x

-- doesn't hold for high impedance
-- doesn't hold for unknown
complements :: (Value -> Value -> Value)
            -> Value -> Known Value
            -> Bool
complements f y (Known x) = f x (complement x) == y

-- doesn't hold for high impedance
doubleNeg :: (Value -> Value)
          -> Known Value -> Bool
doubleNeg f (Known x) = involutory f x

demorgan :: (Value -> Value -> Value)
         -> (Value -> Value -> Value)
         -> (Value -> Value)
         -> Value -> Value
         -> Bool
demorgan f g n x y = f (n x) (n y) == n (g x y)

clearZero :: Int -> Bool
clearZero n = clearBit (zeroBits :: Value) n == zeroBits

setZero :: Int -> Bool
setZero n = setBit (zeroBits :: Value) n == bit n

testZero :: Int -> Bool
testZero n = not (testBit (zeroBits :: Value) n)
-- testZero n = testBit zeroBits n == False

popZero :: Bool
popZero = popCount (zeroBits :: Value) == 0

-- | Num properties
num :: TestTree
num = testGroup "Num Value properties (SmallCheck)"
  [ propsAdd
  , propsMul
  , propsSign
  ]

propsAdd :: TestTree
propsAdd = testGroup "Addition properties"
  [ SC.testProperty "commutativity"
    (TN.commutativeAdd :: Value -> Value -> Bool)
  , SC.testProperty "associativity"
    (TN.associativeAdd :: Value -> Value -> Value -> Bool)
  , SC.testProperty "adding zero" $ knownUnitAdd Zero
  ]

knownUnitAdd :: Value -> Known Value -> Bool
knownUnitAdd zero (Known x) = TN.unitAdd zero x

propsMul :: TestTree
propsMul = testGroup "Multiplication properties"
  [ SC.testProperty "commutativity"
    (TN.commutativeMul :: Value -> Value -> Bool)
  , SC.testProperty "associativity"
    (TN.associativeMul :: Value -> Value -> Value -> Bool)
  , SC.testProperty "multiply by 1" $ knownUnitMul One
  , SC.testProperty "multiply by 0" $ knownAnihilatorMul Zero
  , SC.testProperty "distributivity"
    (TN.distributive :: Value -> Value -> Value -> Bool)
  ]

knownUnitMul :: Value -> Known Value -> Bool
knownUnitMul one (Known x) = TN.unitMul one x

knownAnihilatorMul :: Value -> Known Value -> Known Value -> Bool
knownAnihilatorMul zero (Known x) (Known y) = TN.anihilatorMul zero x y

propsSign :: TestTree
propsSign = testGroup "sign and negation"
  [ SC.testProperty "double negation" knownDoubleNeg
  -- there is no -1 in Value
  -- , SC.testProperty "multiply by -1" $ knownMinusOne One
  , SC.testProperty "abs x × signum x == x" knownSig
  ]

knownDoubleNeg :: Known Value -> Bool
knownDoubleNeg (Known x) = TN.doubleNeg x

knownSig :: Known Value -> Bool
knownSig (Known x) = TN.sig x
