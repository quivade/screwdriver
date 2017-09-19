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

import Control.Monad.Error.Class (MonadError)
import Control.Monad.Trans       (MonadTrans, lift)
import Control.Unification
import Control.Unification.Types
import Data.HashMap.Lazy
import Data.Text                 (Text)

import Language.FIRRTL.Syntax.Annotation
import Language.FIRRTL.Syntax
import Language.FIRRTL.Syntax.Recursion
import Language.FIRRTL.Syntax.Types

type Type' v = UTerm TypeF v
type Expr' v = AnnFix (Type' v) ExprF
type Expr1 = Fix ExprF

data Error t v
  = UndefinedVar Ident v
  | UndefindedTyVar v
  | UFailure (UFailure t v)

deriving instance (Show (t (UTerm t v)), Show v) => Show (Error t v)

data NeedsFreshening = NeedsFreshening | SoFreshAlready

instance Fallible TypeF v (Error TypeF v) where
  occursFailure a b = UFailure $ occursFailure a b
  mismatchFailure a b = UFailure $ mismatchFailure a b

allocateTypeVars :: forall t v m . (BindingMonad t (Type' v) m, t ~ TypeF)
                 => Expr' v -> m (Expr' v)
allocateTypeVars =
  ymapM (\e -> modifyAnn <$> freeVar <*> pure e)

constrain :: ( BindingMonad t v m
             , Fallible t v e
             , MonadTrans em
             , Functor (em m)
             , MonadError e (em m)
             , t ~ TypeF
             , Show v
             )
          => HashMap Text (NeedsFreshening, Type' v) -> Expr' v -> em m (Type' v)
constrain env (Fix (AnnF ty expr)) = do
  ty' <- go expr
  lift $ bindVar ty ty'
  pure ty'
    where go 

