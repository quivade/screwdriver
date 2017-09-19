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
  , TypeF (..)
  , Field (..)
  , Ground (..)
  , Orientation (..)
  ) where

import Control.Unification (UTerm (..))
import Language.FIRRTL.Syntax (Ident)

-- | Ground types
data Ground
  = Unsigned (Maybe Int) -- ^ Unsigned integer with bit width
  | Signed   (Maybe Int) -- ^ Signed integer with bit width
  | Clock                -- ^ Clock
  deriving (Eq, Show)

-- a -> b -> c
-- UInt 2 -> UInt 2 -> SInt 4
-- (UInt a). a -> b -> -> b
data Scheme = Forall [TVar] Type

-- | Firrtl types
data TypeF t
  = Ground Ground    -- ^ Ground type
  | Vector t Int     -- ^ Vector type
  | Bundle [Field t] -- ^ Bundle type
  deriving (Functor, Foldable, Traversable, Eq, Show)

-- in Control.Unification
-- data UTerm t
--   = UVar Ident            -- ^ Type variable
--   | UTerm (t (UTerm t)) -- ^ Type term

type Type  = UTerm TypeF

-- | Single field in the bundle type
data Field f = Field
  { _fieldOrientation :: Orientation
  , _fieldName :: Ident
  , _fieldType :: f
  } deriving (Functor, Foldable, Traversable, Eq, Show)

-- | Fiels orientation
data Orientation = Direct | Flipped
  deriving (Eq, Show)
