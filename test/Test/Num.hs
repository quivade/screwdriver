{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Test.Num where

import           Test.Invariant
import           Test.Tasty               (TestName, TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC
import Test.SmallCheck.Series

newtype Known a = Known { getKnown :: a }

instance Show a => Show (Known a) where
  show (Known a) = show a

quickPropsNum :: (Eq a, Num a, Show a, QC.Arbitrary a)
              => a -> a -> TestTree
quickPropsNum zero one = testGroup "Num properties (QuickCheck)"
  [ quickPropsAdd zero
  , quickPropsMul zero one
  , quickPropsSign one
  ]

smallPropsNum :: (Eq a, Num a, Show a, Serial IO a)
              => a -> a -> TestTree
smallPropsNum zero one = testGroup "Num properties (SmallCheck)"
  [ smallPropsAdd zero
  , smallPropsMul zero one
  , smallPropsSign one
  ]

smallPropsAdd :: (Eq a, Num a, Show a, Serial IO a)
              => a
              -> TestTree
smallPropsAdd zero = testGroup "Addition properties"
  [ SC.testProperty "commutativity" $ commutativeAdd zero
  , SC.testProperty "associativity" $ associativeAdd zero
  , SC.testProperty "adding zero" $ unitAdd zero
  ]

quickPropsAdd :: (Eq a, Num a, Show a, QC.Arbitrary a)
              => a
              -> TestTree
quickPropsAdd zero = testGroup "Addition properties"
  [ QC.testProperty "commutativity" $ commutativeAdd zero
  , QC.testProperty "associativity" $ associativeAdd zero
  , QC.testProperty "adding zero" $ unitAdd zero
  ]

smallPropsMul :: (Eq a, Num a, Show a, Serial IO a)
              => a -> a -> TestTree
smallPropsMul zero one = testGroup "Multiplication properties"
  [ SC.testProperty "commutativity" $ commutativeMul zero
  , SC.testProperty "associativity" $ associativeMul zero
  , SC.testProperty "multiply by 1" $ unitMul one
  , SC.testProperty "multiply by 0" $ anihilatorMul one
  , SC.testProperty "distributivity" $ distributive zero
  ]

quickPropsMul :: (Eq a, Num a, Show a, QC.Arbitrary a)
              => a -> a -> TestTree
quickPropsMul zero one = testGroup "Multiplication properties"
  [ QC.testProperty "commutativity" $ commutativeMul zero
  , QC.testProperty "associativity" $ associativeMul zero
  , QC.testProperty "multiply by 1" $ unitMul one
  , QC.testProperty "multiply by 0" $ anihilatorMul one
  , QC.testProperty "distributivity" $ distributive zero
  ]

smallPropsSign :: (Eq a, Num a, Show a, Serial IO a)
               => a
               -> TestTree
smallPropsSign one = testGroup "sign and negation"
  [ SC.testProperty "double negation" $ doubleNeg one
  , SC.testProperty "multiply by -1" $ minusOne one
  , SC.testProperty "abs x × signum x == x" $ sig one
  ]

quickPropsSign :: (Eq a, Num a, Show a, QC.Arbitrary a)
               => a
               -> TestTree
quickPropsSign one = testGroup "sign and negation"
  [ QC.testProperty "double negation" $ doubleNeg one
  , QC.testProperty "multiply by -1" $ minusOne one
  , QC.testProperty "abs x × signum x == x" $ sig one
  ]

unitElement :: Eq a => (a -> a -> a) -> a -> a -> Bool
unitElement f u e = f e u == e

-- -- | Addition properties
commutativeAdd :: (Eq a, Num a) => a -> a -> Bool
commutativeAdd = commutative (+)

associativeAdd :: (Eq a, Num a) => a -> a -> a -> Bool
associativeAdd = associative (+)

unitAdd :: (Eq a, Num a) => a -> a -> Bool
unitAdd = unitElement (+)

-- | Multiplication properties
commutativeMul :: (Eq a, Num a) => a -> a -> Bool
commutativeMul = commutative (*)

associativeMul :: (Eq a, Num a) => a -> a -> a -> Bool
associativeMul = associative (*)

distributive :: (Eq a, Num a) => a -> a -> a -> Bool
distributive = distributesOver (*) (+)

unitMul :: (Eq a, Num a) => a -> a -> Bool
unitMul = unitElement (*)

anihilatorMul :: (Eq a, Num a) => a -> a -> a -> Bool
anihilatorMul = fixedBy (*)

-- | Negation
doubleNeg :: (Eq a, Num a) => a -> Bool
doubleNeg = involutory negate

-- -- | -1 * x = -x
minusOne :: (Eq a, Num a) => a -> a -> Bool
minusOne one x = negate one * x == negate x

sig :: (Eq a, Num a) => a -> Bool
sig x = abs x * signum x == x
