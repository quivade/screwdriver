{-# language
      FlexibleInstances
    , UndecidableInstances #-}
{-|
Copyright   : (c) Quivade, 2017
License     : GPL-3
Maintainer  : Jakub Kopański <jakub@quivade.com>
Stability   : experimental
Portability : POSIX

This module provides FIRRTL Types definitions.
|-}
module Language.FIRRTL.Unification
  where

import Control.Monad.Error.Class  (MonadError)
import Control.Monad.Trans        (MonadTrans, lift)
import Control.Unification        (BindingMonad (..), Fallible (..))
import Control.Unification.IntVar (IntVar)
import Control.Unification.Types  (UFailure, UTerm (..))
import Data.HashMap.Lazy
import Data.Text                  (Text)

import Language.FIRRTL.Annotations
import Language.FIRRTL.Recursion
import Language.FIRRTL.Syntax
import Language.FIRRTL.Types

data Error t v
  = UndefinedVar Ident v
  | UndefindedTyVar v
  | UFailure (UFailure t v)

deriving instance (Show (t (UTerm t v)), Show v) => Show (Error t v)

instance Fallible TypeF v (Error TypeF v) where
  occursFailure a b = UFailure $ occursFailure a b
  mismatchFailure a b = UFailure $ mismatchFailure a b

allocateVar :: forall m. BindingMonad TypeF IntVar m
            => Maybe Type -> m PolyType
allocateVar Nothing = UVar <$> freeVar
allocateVar (Just t) = pure (poly t)

cataM :: (Traversable f, Monad m) => (f a -> m a) -> Fix f -> m a
cataM = undefined

genTypeVars :: forall m. (BindingMonad TypeF IntVar m)
            => TypedExpr -> m PolyTypedExpr
genTypeVars = cataM alg
  where alg :: forall m. (BindingMonad TypeF IntVar m, Monad m)
            => (Ann (Maybe Type) ExprF PolyTypedExpr) -> m (AnnFix PolyType ExprF)
        alg (AnnF mtype expr) = annotate <$> (allocateVar mtype) >>= \f -> return $ f expr
