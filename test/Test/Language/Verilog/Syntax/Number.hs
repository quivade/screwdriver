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
import Test.Language.Verilog.Syntax.Number.Value

arbitraryValues :: Arbitrary a => Gen a -> Int -> Gen [a]
arbitraryValues gen m
  | m == 0 = return []
  | otherwise = (:) <$> gen <*> arbitraryValues gen (m - 1)

instance Arbitrary Number where
  arbitrary = QC.sized $ \n -> do
    s <- choose (True, False)
    let sign = if s then Unsigned
                    else Signed
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
