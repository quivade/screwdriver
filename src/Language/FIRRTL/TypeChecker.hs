{-# LANGUAGE FlexibleContexts #-}
module Language.FIRRTL.TypeChecker where

import Control.Monad.Except
import Control.Monad.Reader

import Language.FIRRTL.Syntax

data TypeError
  = TypeMismatch Type Type
  | NotInScope Ident
  | NotABundle Type Ident
  | NotAClock Type
  | NotAVector Type
  | NoField Ident
  | MultipleFields Ident
  | OutOfBounds Int
  | PositiveInteger Int
  | NonNegativeInteger Int

instance Show TypeError where
  show (TypeMismatch got expected) =
    "Expected: " ++ show expected ++ " but got: " ++ show got
  show (NotInScope name) =
    "Could not find reference: " ++ show name
  show (NotABundle got field) =
    "Expected Bundle type with " ++ show field ++ " field, but got "
    ++ show got ++ " instead"
  show (NotAVector got) =
    "Expected a Vector type, but got " ++ show got ++ " instead"
  show (NoField field) =
    "Could not find field " ++ show field
  show (MultipleFields field) =
    "Multiple fields " ++ show field ++ " defined in a bundle"
  show (OutOfBounds idx) =
    "Index " ++ show idx ++ " is out of bounds"
  show (NotAClock got) =
    "Expected Clock type, but got " ++ show got ++ " instead"
  show (PositiveInteger i) =
    "Expected positive integer, but got " ++ show i ++ " instead"
  show (NonNegativeInteger i) =
    "Expected non negative integer, but got " ++ show i ++ " instead"

type Check = ExceptT TypeError (Reader Env)

type Env = [(Ident, Type)]

extend :: (Ident, Type) -> Env -> Env
extend xt env = xt : env

inEnv :: (Ident, Type) -> Check a -> Check a
inEnv (x, t) = local (extend (x, t))

lookupRef :: Ident -> Check Type
lookupRef x = do
  env <- ask
  case lookup x env of
    Just e -> return e
    Nothing -> throwError $ NotInScope x

check :: Expr -> Check Type
check (Lit g) =
  pure $ case g of
    (SInt w _) -> Signed w
    (UInt w _) -> Unsigned w
    Clk -> Clock

check (Valid cond value) = do
  tc <- check cond
  let u = Unsigned 1
  unless (tc == u) (throwError $ TypeMismatch tc u)
  check value

check (Mux cond a b) = do
  tc <- check cond
  let u = Unsigned 1
  unless (tc == u) (throwError $ TypeMismatch tc u)
  ta <- check a
  tb <- check b
  if ta == tb
     then pure ta
     else throwError $ TypeMismatch ta tc

check (Ref name) = lookupRef name

check (SubField expr name) = do
  te <- check expr
  case te of
    (Bundle fields) ->
      let a = filter (\field -> _fieldName field == name) fields
          n = length a
          f n | n < 1 = throwError $ NoField name
              | n > 1 = throwError $ MultipleFields name
              | otherwise = pure $ _fieldType $ head a
       in f n
    x -> throwError $ NotABundle te name

check (SubIndex expr idx) = do
  te <- check expr
  case te of
    (Vector t len) ->
      if (idx > 0) && (idx < len)
         then pure t
         else throwError $ OutOfBounds idx
    x -> throwError $ NotAVector te

check (SubAccess expr idx) = do
  ti <- check idx
  case ti of
    (Unsigned w) -> check (SubIndex expr $ 2 ^ w - 1)
    _ -> throwError $ TypeMismatch ti (Unsigned 1)

-- TODO: implement
check (Op _) = undefined

statementCheck :: Statement -> Check Type
statementCheck (Block stmts) = last (statementCheck <$> stmts)
statementCheck (Cond test cons alt) = do
  tt <- check test
  let u = Unsigned 1
  unless (tt == u) (throwError $ TypeMismatch tt u)
  pure Unit

statementCheck (Connect lhs rhs) = do
  tl <- check lhs
  tr <- check rhs
  unless (tl == tr) (throwError $ TypeMismatch tr tl)
  pure tl

statementCheck Empty = pure Unit
statementCheck (Instance name ident) = pure Unit
statementCheck (Invalid expr) = check expr
statementCheck (Mem dataType depth wLatency rLatency _ _ _ _) = do
  unless (depth > 0) (throwError $ PositiveInteger depth)
  unless (rLatency >= 0) (throwError $ NonNegativeInteger rLatency)
  unless (wLatency >= 0) (throwError $ NonNegativeInteger wLatency)
  pure dataType

statementCheck (Node _ expr) = check expr
-- FIXME: this is same as connect right now
statementCheck (Partial lhs rhs) = do
  tl <- check lhs
  tr <- check rhs
  unless (tl == tr) (throwError $ TypeMismatch tr tl)
  pure tl

statementCheck (Print clk test _ _) = do
  tc <- check clk
  unless (tc == Clock) (throwError $ NotAClock tc)
  tt <- check test
  unless (tt == Unsigned 1) (throwError $ TypeMismatch tt (Unsigned 1))
  pure Unit

statementCheck (Reg t _ clk rst init) = do
  tc <- check clk
  unless (tc == Clock) (throwError $ NotAClock tc)
  tr <- check rst
  let u = Unsigned 1
  unless (tr == u) (throwError $ TypeMismatch tr u)
  ti <- check init
  unless (t == ti) (throwError $ TypeMismatch ti t)
  pure t

statementCheck (Stop _ clk halt) = do
  tc <- check clk
  unless (tc == Clock) (throwError $ NotAClock tc)
  tt <- check halt
  unless (tt == Unsigned 1) (throwError $ TypeMismatch tt (Unsigned 1))
  pure Unit

statementCheck (Wire t ident) = pure t

-- data Gender = Male | Female | Bi
--   deriving (Eq, Show)

-- gender :: Expr -> Gender
-- gender (Lit g) = 
