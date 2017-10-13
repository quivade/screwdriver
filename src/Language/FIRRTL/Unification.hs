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

import           Control.Monad.Error.Class    (MonadError, throwError)
import           Control.Monad.Trans          (MonadTrans, lift)
import           Control.Monad.Trans.Except   (ExceptT, runExceptT)
import           Control.Unification          (BindingMonad (..), applyBindingsAll, unify)
import           Control.Unification.IntVar   (IntBindingT, IntVar)
import           Control.Unification.Types    ( Fallible (..)
                                              , UFailure (..)
                                              , Unifiable (..)
                                              , UTerm (..)
                                              )
import           Data.Foldable                (toList)
import           Data.Functor.Identity        (Identity)
import qualified Data.HashMap.Strict          as Map
import           Data.HashMap.Strict          (HashMap)
import           Data.Text                    (Text)

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

instance Unifiable TypeF where
  -- zipMatch :: TypeF a -> TypeF a -> Maybe (t Either a (a, a))
  zipMatch (Ground g) (Ground h) = Ground <$> match g h where
    match (Unsigned (Just n)) (Unsigned (Just m)) =
      if n == m then Just $ Unsigned (Just n) else Nothing
    match (Unsigned Nothing) (Unsigned mw)      = Just $ Unsigned mw
    match (Unsigned mw)      (Unsigned Nothing) = Just $ Unsigned mw
    match (Signed (Just n)) (Signed (Just m)) =
      if n == m then Just $ Signed (Just n) else Nothing
    match (Signed Nothing) (Signed mw) = Just $ Signed mw
    match (Signed mw) (Signed Nothing) = Just $ Signed mw
    match Clock Clock = Just Clock
    match _ _ = Nothing
    -- |
  zipMatch (Vector p n) (Vector q m) = if n /= m then Nothing
    else Just $ Vector (Right (p, q)) n
  zipMatch (Bundle fa) (Bundle fb) = Bundle <$> matchList fa fb
    where
      matchList la lb
        | length la == length lb =
          sequence $ zipWith zipMatch la lb
        | otherwise = Nothing
  zipMatch _ _ = Nothing

instance Unifiable Field where
  zipMatch (Field ol nl tl) (Field or nr tr)
    | ol == or && nl == nr = Just $ Field ol nr (Right (tl, tr))
    | otherwise = Nothing

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

allTypes :: PolyTypedExpr -> [PolyType]
allTypes = cata alg
  where alg :: Ann PolyType ExprF [PolyType] -> [PolyType]
        alg = concat . toList

typecheck :: TypedExpr
          -> ExceptT (Error TypeF IntVar) (IntBindingT TypeF Identity) PolyTypedExpr
typecheck expr = do
  pexpr <- lift $ genTypeVars expr
  constrained <- constrain mempty pexpr
  applyBindingsAll (allTypes pexpr)
  pure pexpr
