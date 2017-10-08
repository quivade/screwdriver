{-# language GADTs #-}
{-|
Copyright   : (c) Quivade, 2017
License     : GPL-3
Maintainer  : Jakub Kopański <jakub@quivade.com>
Stability   : experimental
Portability : POSIX

This module provides FIRRTL Types definitions.
|-}
module Language.FIRRTL.Types
  ( Type
  , PolyType
  , TypeF (..)
  , Field (..)
  , Ground (..)
  , Orientation (..)
  , poly
  , ground
  , vector
  , bundle
  ) where

import Control.Unification           (UTerm (..), Variable (..))
import Control.Unification.IntVar    (IntVar)

import Language.FIRRTL.Recursion     (Fix (..), cata)
import Language.FIRRTL.Syntax.Common (Ident)

-- | Ground types
data Ground
  = Unsigned (Maybe Int) -- ^ Unsigned integer with bit width
  | Signed   (Maybe Int) -- ^ Signed integer with bit width
  | Clock                -- ^ Clock
  | Natural              -- ^ Non negative integer
  deriving (Eq, Show)

-- data Scheme where
--   Forall :: Variable v => [v] -> Type v -> Scheme

data Scheme = Forall [IntVar] Type

-- | Firrtl types
data TypeF t
  = Ground Ground    -- ^ Ground type
  | Vector t Int     -- ^ Vector type
  | Bundle [Field t] -- ^ Bundle type
  deriving (Functor, Foldable, Traversable, Eq, Show)

-- | Single field in the bundle type
data Field f = Field
  { _fieldOrientation :: Orientation
  , _fieldName :: Ident
  , _fieldType :: f
  } deriving (Functor, Foldable, Traversable, Eq, Show)

-- | Fiels orientation
data Orientation = Direct | Flipped
  deriving (Eq, Show)

-- Fix   (TypeF (Fix   TypeF))
type Type  = Fix TypeF
-- UTerm (TypeF (UTerm TypeF IntVar))
type PolyType  = UTerm TypeF IntVar

ground :: Ground -> PolyType
ground = UTerm . Ground

vector :: PolyType -> Int -> PolyType
vector t n = UTerm $ Vector t n

bundle :: [Field PolyType] -> PolyType
bundle = UTerm . Bundle

-- poly :: Type -> PolyType
-- poly (Fix (Ground t)) = ground t
-- poly (Fix (Vector t n)) = vector (poly t) n
-- poly (Fix (Bundle fs)) = bundle ((\f -> f { _fieldType = poly $ _fieldType f }) <$> fs)

poly :: Type -> PolyType
poly = cata alg
  where alg (Ground t) = ground t
        alg (Vector t n) = vector t n
        alg (Bundle fs) = bundle fs
