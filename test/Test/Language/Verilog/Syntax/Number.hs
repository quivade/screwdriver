{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Test.Language.Verilog.Syntax.Number where

import Data.Bits
import Test.Invariant
import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC
import Test.QuickCheck (Arbitrary, Gen, arbitrary, choose, frequency, shrink)
import Test.SmallCheck.Series

import Language.Verilog.Syntax.Number
import Language.Verilog.Syntax.Number.Value
import qualified Test.Language.Verilog.Syntax.Number.Value as TV

import Test.Num (Known (..))
import qualified Test.Num as TN

arbitraryValues :: Arbitrary a => Gen a -> Int -> Gen [a]
arbitraryValues gen m
  | m == 0 = return []
  | otherwise = (:) <$> gen <*> arbitraryValues gen (m - 1)

instance Arbitrary Sign where
  arbitrary = choose (True, False)
    >>= \s -> return $
      if s then Unsigned
           else Signed

  shrink Unsigned = []
  shrink Signed = [Unsigned]

instance Arbitrary Number where
  arbitrary = QC.sized $ \n -> do
    sign <- arbitrary :: Gen Sign
    known <- frequency [ (7, return 0)
                       , (2, return 1)
                       ]
    let gen = case known of
          0 -> (arbitrary :: Gen (Known Value)) >>= \(Known a) -> return a
          1 -> arbitrary :: Gen Value
    val <- arbitraryValues gen n
    return $ Number sign n val

  shrink (Number sign _ val) = do
    v <- shrink val
    return $ Number sign (length v) v

instance Arbitrary (Known Number) where
  arbitrary = QC.sized $ \n -> do
    sign <- arbitrary :: Gen Sign
    let gen = (arbitrary :: Gen (Known Value)) >>= \(Known a) -> return a
    val <- arbitraryValues gen n
    return . Known $ Number sign n val

  shrink (Known n) = Known <$> shrink n

instance Monad m => Serial m Sign where
  series = cons0 Unsigned \/ cons0 Signed

numbers :: [Value] -> Depth -> [Number]
numbers set d = (signed <$> values) ++ (unsigned <$> values)
  where unsigned = Number Unsigned d
        signed = Number Signed d
        values = go d [[]]
        go :: Depth -> [[Value]] -> [[Value]]
        go 0 vs = vs
        go n vs = go (n-1) $ (:) <$> set <*> vs

instance Monad m => Serial m Number where
  series = generate (numbers [Zero, One, Unknown, HighZ])

instance Monad m => Serial m (Known Number) where
  series = Known <$> generate (numbers [Zero, One])

properties :: TestTree
properties = testGroup "Number Num instance"
  [smallNum]

-- | Num properties
smallNum :: TestTree
smallNum = testGroup "Num Number properties (SmallCheck)"
  [ smallPropsAdd
  , smallPropsMul
  -- , propsSign
  ]

limitBits :: SC.Testable m a => SC.Depth -> a -> SC.Property m
limitBits n = SC.changeDepth (const n)

smallPropsAdd :: TestTree
smallPropsAdd = testGroup "Addition properties"
  [ SC.testProperty "commutativity"
    $ limitBits 4 (TN.commutativeAdd :: Number -> Number -> Bool)
  , SC.testProperty "associativity"
    $ limitBits 3 (TN.associativeAdd :: Number -> Number -> Number -> Bool)
  , SC.testProperty "adding zero"
    -- Unsigned Zero acctually changes expression type to Unsigned
    -- Signed Zero is neutral in that matter
    $ limitBits 4 (knownUnitAdd (Number Signed 1 [Zero]))
  ]

smallPropsMul :: TestTree
smallPropsMul = testGroup "Multiplication properties"
  [ SC.testProperty "commutativity"
    $ limitBits 4 (TN.commutativeMul :: Number -> Number -> Bool)
  , SC.testProperty "associativity"
    $ limitBits 3 (TN.associativeMul :: Number -> Number -> Number -> Bool)
  , SC.testProperty "multiply by 1"
    $ limitBits 4 (TN.unitMul (fromInteger 1) :: Number -> Bool)
  , SC.testProperty "multiply by 0"
    $ limitBits 4 (
      (TN.anihilatorMul $ Number Unsigned 1 [Zero]) :: Number -> Number -> Bool
    )
  , SC.testProperty "distributivity"
    $ limitBits 4 (TN.distributive :: Number -> Number -> Number -> Bool)
  ]


knownUnitAdd :: Number -> Known Number -> Bool
knownUnitAdd zero (Known x) = TN.unitAdd zero x
