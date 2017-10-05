{-# language StandaloneDeriving #-}
{-# language UndecidableInstances #-}
{-|
Copyright   : (c) Quivade, 2017
License     : GPL-3
Maintainer  : Jakub Kopański <jakub@quivade.com>
Stability   : experimental
Portability : POSIX

This module provides basic recursion schemes.
Written for exercise, could possibly be replaced by
[recursion-schemes](https://hackage.haskell.org/package/recursion-schemes).
|-}
module Language.FIRRTL.Recursion
  ( Fix (..)
  , unfix
  , cata
  , hmap
  , ymap
  , hmapM
  , ymapM
  , Algebra
  , Coalgebra
  , ana
  -- , TFix (..)
  ) where

import Control.Monad ((<=<), liftM)

newtype Fix f = Fix (f (Fix f))
deriving instance Show (f (Fix f)) => Show (Fix f)
deriving instance Eq (f (Fix f)) => Eq (Fix f)

unfix :: Fix f -> f (Fix f)
unfix (Fix f) = f

hmap :: (Functor f, Functor g) => (forall a. f a -> g a) -> Fix f -> Fix g
hmap eps = ana (eps . unfix)

hmapM :: (Functor f, Traversable t, Monad m)
      => (forall a. f a -> m (t a)) -> Fix f -> m (Fix t)
hmapM eps = anaM (eps . unfix)

ymap :: Functor f => (Fix f -> Fix f) -> Fix f -> Fix f
ymap f = Fix . fmap f . unfix

ymapM :: (Traversable f, Monad m)
      => (Fix f -> m (Fix f)) -> Fix f -> m (Fix f)
ymapM f = liftM Fix . mapM f . unfix

type Algebra f a = f a -> a
type Coalgebra f a = a -> f a

-- * catamorphism
cata :: Functor f => Algebra f a -> Fix f -> a
cata f = f  . fmap (cata f) . unfix

-- * anamorphism
ana :: Functor f => Coalgebra f a -> a -> Fix f
ana psi = Fix . fmap (ana psi) . psi

anaM :: (Traversable f, Monad m) => (a -> m (f a)) -> (a -> m (Fix f))
anaM psiM = self
  where self = (fmap Fix . mapM self) <=< psiM

-- don't know why frank need this
-- type TFix
--   (t :: (* -> *) -> (* -> *))
--   (f :: ((* -> *) -> (* -> *)) -> * -> *)
--   = Fix (t (f t))
