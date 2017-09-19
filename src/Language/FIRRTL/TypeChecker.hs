{-# LANGUAGE
      FlexibleContexts,
      FlexibleInstances,
      GeneralizedNewtypeDeriving,
      MultiParamTypeClasses,
      StandaloneDeriving,
      UndecidableInstances #-}
module Language.FIRRTL.TypeChecker where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State.Strict
import Data.HashMap.Lazy

import Language.FIRRTL.Syntax

newtype FreshT m a = Fresh { runFreshT :: StateT Integer m a }

-- (a -> b) -> FreshT m a -> FreshT m b
instance Functor m => Functor (FreshT m) where
  fmap f = Fresh . fmap f . runFreshT

instance (Applicative m, Monad m) => Applicative (FreshT m) where
  pure = Fresh . pure
  -- FreshT m (a -> b) -> FreshT m a -> FreshT m b
  (<*>) f a = Fresh (runFreshT f <*> runFreshT a)

instance (Applicative m, Monad m) => Monad (FreshT m) where
  return = pure
  -- FreshT m a -> (a -> FreshT m b) -> FreshT m b
  (>>=) a f = Fresh (runFreshT a >>= runFreshT . f)

instance MonadState Integer m => MonadState Integer (FreshT m) where
  -- m Integer
  get = get
  put = put
  state = state

instance MonadTrans FreshT where
  lift = Fresh . lift

instance MonadError e m => MonadError e (FreshT m) where
  throwError = lift . throwError
  catchError m h = Fresh $ catchError (runFreshT m) (runFreshT . h)

evalFreshT :: Monad m => FreshT m a -> m a
evalFreshT m = evalStateT (runFreshT m) 0

evalFresh :: FreshT Identity a -> a
evalFresh m = evalState (runFreshT m) 0

class Monad m => Fresh m where
  fresh :: m Integer

instance MonadState Integer m => Fresh (FreshT m) where
  fresh = do n <- get
             put $ n + 1
             return n

instance Fresh m => Fresh (StateT s m) where
  fresh = lift fresh

newtype Infering a = Infering
  { runInfering :: StateT Context (FreshT (Except Error)) a }

data Scheme = Forall [Ident] Type

-- | Map type variable to a inferred type
data Context = HashMap Ident Scheme

deriving instance Functor Infering
deriving instance Applicative Infering
deriving instance Monad Infering
deriving instance MonadState Context Infering
deriving instance MonadError Error Infering
-- deriving instance Fresh Infering

data Error = Mismatch Type Type
-- data TypeError
--   = TypeMismatch Type Type
--   | NotInScope Ident
--   | NotABundle Type Ident
--   | NotAClock Type
--   | NotAVector Type
--   | NoField Ident
--   | MultipleFields Ident
--   | OutOfBounds Int
--   | PositiveInteger Int
--   | NonNegativeInteger Int

-- instance Show TypeError where
--   show (TypeMismatch got expected) =
--     "Expected: " ++ show expected ++ " but got: " ++ show got
--   show (NotInScope name) =
--     "Could not find reference: " ++ show name
--   show (NotABundle got field) =
--     "Expected Bundle type with " ++ show field ++ " field, but got "
--     ++ show got ++ " instead"
--   show (NotAVector got) =
--     "Expected a Vector type, but got " ++ show got ++ " instead"
--   show (NoField field) =
--     "Could not find field " ++ show field
--   show (MultipleFields field) =
--     "Multiple fields " ++ show field ++ " defined in a bundle"
--   show (OutOfBounds idx) =
--     "Index " ++ show idx ++ " is out of bounds"
--   show (NotAClock got) =
--     "Expected Clock type, but got " ++ show got ++ " instead"
--   show (PositiveInteger i) =
--     "Expected positive integer, but got " ++ show i ++ " instead"
--   show (NonNegativeInteger i) =
--     "Expected non negative integer, but got " ++ show i ++ " instead"

-- type Check t = ExceptT TypeError t
-- type Context = HashMap Ident Type

-- data Env = Env
--   { ctx :: Context
--   } deriving (Eq, Show)

-- lookupRef :: Env -> Ident -> Check Type
-- lookupRef e x =
--   let c = ctx e
--    in case lookup x c of
--     Just e -> return e
--     Nothing -> throwError $ NotInScope x

-- extend :: Env -> Ident -> Type -> Env
-- extend env id t = if member id env
--                      then throwError $ AlreadyDefined id
--                      else env { ctx = insert id t (ctx env) }

-- -- check :: Env -> Expr -> Check Type
-- -- check (Lit g) =
-- --   pure $ case g of
-- --     (SInt w _) -> Signed w
-- --     (UInt w _) -> Unsigned w
-- --     Clk -> Clock

-- -- check (Valid cond value) = do
-- --   tc <- check cond
-- --   let u = Unsigned 1
-- --   unless (tc == u) (throwError $ TypeMismatch tc u)
-- --   check value

-- -- check (Mux cond a b) = do
-- --   tc <- check cond
-- --   let u = Unsigned 1
-- --   unless (tc == u) (throwError $ TypeMismatch tc u)
-- --   ta <- check a
-- --   tb <- check b
-- --   if ta == tb
-- --      then pure ta
-- --      else throwError $ TypeMismatch ta tc

-- -- check (Ref name) = lookupRef name

-- -- check (SubField expr name) = do
-- --   te <- check expr
-- --   case te of
-- --     (Bundle fields) ->
-- --       let a = filter (\field -> _fieldName field == name) fields
-- --           n = length a
-- --           f n | n < 1 = throwError $ NoField name
-- --               | n > 1 = throwError $ MultipleFields name
-- --               | otherwise = pure $ _fieldType $ head a
-- --        in f n
-- --     x -> throwError $ NotABundle te name

-- -- check (SubIndex expr idx) = do
-- --   te <- check expr
-- --   case te of
-- --     (Vector t len) ->
-- --       if (idx > 0) && (idx < len)
-- --          then pure t
-- --          else throwError $ OutOfBounds idx
-- --     x -> throwError $ NotAVector te

-- -- check (SubAccess expr idx) = do
-- --   ti <- check idx
-- --   case ti of
-- --     (Unsigned w) -> check (SubIndex expr $ 2 ^ w - 1)
-- --     _ -> throwError $ TypeMismatch ti (Unsigned 1)

-- -- -- TODO: implement
-- -- check (Op _) = undefined

-- -- statementCheck :: Statement -> Check Type
-- -- statementCheck (Block stmts) = last (statementCheck <$> stmts)
-- -- statementCheck (Cond test cons alt) = do
-- --   tt <- check test
-- --   let u = Unsigned 1
-- --   unless (tt == u) (throwError $ TypeMismatch tt u)
-- --   pure Unit

-- -- statementCheck (Connect lhs rhs) = do
-- --   tl <- check lhs
-- --   tr <- check rhs
-- --   unless (tl == tr) (throwError $ TypeMismatch tr tl)
-- --   pure tl

-- -- statementCheck Empty = pure Unit
-- -- statementCheck (Instance name ident) = pure Unit
-- -- statementCheck (Invalid expr) = check expr
-- -- statementCheck (Mem dataType depth wLatency rLatency _ _ _ _) = do
-- --   unless (depth > 0) (throwError $ PositiveInteger depth)
-- --   unless (rLatency >= 0) (throwError $ NonNegativeInteger rLatency)
-- --   unless (wLatency >= 0) (throwError $ NonNegativeInteger wLatency)
-- --   pure dataType

-- -- statementCheck (Node _ expr) = check expr
-- -- -- FIXME: this is same as connect right now
-- -- statementCheck (Partial lhs rhs) = do
-- --   tl <- check lhs
-- --   tr <- check rhs
-- --   unless (tl == tr) (throwError $ TypeMismatch tr tl)
-- --   pure tl

-- -- statementCheck (Print clk test _ _) = do
-- --   tc <- check clk
-- --   unless (tc == Clock) (throwError $ NotAClock tc)
-- --   tt <- check test
-- --   unless (tt == Unsigned 1) (throwError $ TypeMismatch tt (Unsigned 1))
-- --   pure Unit

-- -- statementCheck (Reg t _ clk rst init) = do
-- --   tc <- check clk
-- --   unless (tc == Clock) (throwError $ NotAClock tc)
-- --   tr <- check rst
-- --   let u = Unsigned 1
-- --   unless (tr == u) (throwError $ TypeMismatch tr u)
-- --   ti <- check init
-- --   unless (t == ti) (throwError $ TypeMismatch ti t)
-- --   pure t

-- -- statementCheck (Stop _ clk halt) = do
-- --   tc <- check clk
-- --   unless (tc == Clock) (throwError $ NotAClock tc)
-- --   tt <- check halt
-- --   unless (tt == Unsigned 1) (throwError $ TypeMismatch tt (Unsigned 1))
-- --   pure Unit

-- -- statementCheck (Wire t ident) = pure t

-- -- -- data Gender = Male | Female | Bi
-- -- --   deriving (Eq, Show)

-- -- -- gender :: Expr -> Gender
-- -- -- gender (Lit g) = 
