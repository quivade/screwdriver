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

import           Control.Monad.Error.Class  (MonadError, throwError)
import           Control.Monad.Trans        (MonadTrans, lift)
import           Control.Unification        (BindingMonad (..), unify)
import           Control.Unification.IntVar (IntVar)
import           Control.Unification.Types  (Fallible (..), UFailure (..), UTerm (..))
import qualified Data.HashMap.Strict        as Map
import           Data.HashMap.Strict        (HashMap)
import           Data.Text                  (Text)

import Language.FIRRTL.Annotations
import Language.FIRRTL.Recursion
import Language.FIRRTL.Syntax
import Language.FIRRTL.Types

data Error t v
  = UndefinedIdent Ident
  | UFailure (UFailure t v)

deriving instance (Show (t (UTerm t v)), Show v) => Show (Error t v)

instance Fallible TypeF IntVar (Error TypeF IntVar) where
  occursFailure a b = UFailure $ occursFailure a b
  mismatchFailure a b = UFailure $ mismatchFailure a b

-- instance Fallible TypeF v (Error TypeF v) where
--   occursFailure a b = UFailure $ occursFailure a b
--   mismatchFailure a b = UFailure $ mismatchFailure a b

-- instance Unifable TypeF where
--   zipMatch :: TypeF a -> TypeF a -> Maybe (t Either a (a, a))
  -- zipMatch (Ground g) (Ground h) = match g h
  --   where match a b
  --   | a == Unsigned (Just n) && b == Unsigned (Just m) =
  --       if n == m then Just $ Unsigned (Just n) else Nothing
  --   | a == Unsigned Nothing && b == Unsigned mw = Just $ Unsigned mw
  --   | a == Unsigned mw && b == Unsigned Nothing = Just $ Unsigned mw
  --   | a == Signed (Just n) && b == Signed (Just m) =
  --       if n == m then Just $ Signed (Just n) else Nothing
  --   | a == Signed Nothing && b == Signed mw = Just $ Signed mw
  --   | a == Signed mw && b == Signed Nothing = Just $ Signed mw
  --   | a == Clock && b == Clock == Just Clock
  --   | otherwise = Nothing
  -- -- zipMatch (Vector g n) (Vector h m) =
  --  zipMatch _ _ = Nothing

allocateVar :: forall m. BindingMonad TypeF IntVar m
            => Maybe Type -> m PolyType
allocateVar Nothing = UVar <$> freeVar
allocateVar (Just t) = pure (poly t)

genTypeVars :: forall m. (BindingMonad TypeF IntVar m)
            => TypedExpr -> m PolyTypedExpr
genTypeVars = cataM alg
  where alg :: forall m. (BindingMonad TypeF IntVar m, Monad m)
         -- => Ann (Maybe Type) ExprF PolyTypedExpr -> m (AnnFix PolyType ExprF)
            => Ann (Maybe Type) ExprF PolyTypedExpr -> m PolyTypedExpr
        alg (AnnF mtype expr) = annotate <$> allocateVar mtype
                                         <*> pure expr

type Environment = HashMap Ident PolyType

extend :: Environment -> (Ident, PolyType) -> Environment
extend env (id, ptype) = Map.insert id ptype env

lookupRef :: forall em m. ( BindingMonad TypeF IntVar m, Monad m
                          , Functor (em m), MonadTrans em
                          , MonadError (Error TypeF IntVar) (em m) )
          => Environment -> Ident -> em m PolyType
lookupRef env id = do
  case Map.lookup id env of
    Nothing -> throwError $ UndefinedIdent id
    Just t  -> pure t

constrain :: forall em m. ( BindingMonad TypeF IntVar m, Monad m
                          , Functor (em m), MonadTrans em
                          , MonadError (Error TypeF IntVar) (em m) )
          => Environment -> PolyTypedExpr -> em m PolyTypedExpr
constrain env = cataM alg
  where
    exprType :: forall em m. ( BindingMonad TypeF IntVar m, Monad m
                             , Functor (em m), MonadTrans em
                             , MonadError (Error TypeF IntVar) (em m) )
             => ExprF PolyTypedExpr -> em m PolyType
    exprType (Lit (Nat  n)) = pure $ ground Natural
    exprType (Lit (UInt n)) = pure $ ground (Unsigned Nothing)
    exprType (Lit (SInt n)) = pure $ ground (Signed Nothing)
    -- exprType (Ref id)       = lookupRef env

    alg :: forall em m. ( BindingMonad TypeF IntVar m, Monad m
                        , Functor (em m), MonadTrans em
                        , MonadError (Error TypeF IntVar) (em m) )
        => Ann PolyType ExprF PolyTypedExpr -> em m PolyTypedExpr
    alg (AnnF polytype expr) = case polytype of
      UVar v  -> do
        ty <- exprType expr
        lift $ bindVar v ty
        pure $ annotate (UVar v) expr
      UTerm t -> do
        ty <- exprType expr
        ty' <- unify polytype ty
        annotate <$> (unify polytype ty) <*> pure expr
