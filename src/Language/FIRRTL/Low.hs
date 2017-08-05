{-# LANGUAGE MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleContexts,
             FlexibleInstances,
             UndecidableInstances #-}
{- this is sort of like FIRRTL AST
    but with some restrictions -}
module Language.FIRRTL.Low
  ( -- AST re-exports
    Direction (..)
  , Expr
  , Ident
  , Orientation (..)
  , Reset

  -- AST overrides
  , Circuit (..)
  , Module (..)
  , Port (..)
  , Statement (..)
  , Ctx (Ctx)
  , expandPorts
  , lower
  ) where

import           Language.FIRRTL.Syntax ( Direction (Input, Output)
                                        , Orientation (Direct, Flipped)
                                        , Ident, Expr, Mem, Reset)
import qualified Language.FIRRTL.Syntax as AST

data Ctx = Ctx

class HasLower s a | a -> s where
  lower :: Ctx -> s -> a

data Circuit = Circuit
  { _circuitTop :: Ident
  , _circuitModules :: [Module]
  } deriving Show

instance (HasLower AST.Module Module) =>
  HasLower AST.Circuit Circuit where
    lower ctx (AST.Circuit id modules) = Circuit id (lower ctx <$> modules)

data Module
  = Module
    { _moduleName :: Ident
    , _modulePorts :: [Port]
    , _module :: Statement
    }
  | ExtModule
    { _extModuleName :: Ident
    , _extModulePorts :: [Port]
    }
    deriving Show

instance (HasLower AST.Port Port, HasLower AST.Statement Statement) =>
  HasLower AST.Module Module where
    lower ctx (AST.ExtModule id ports) =
      ExtModule id (lower ctx <$> concatMap expandPorts ports)
    lower ctx (AST.Module id ports stmt) =
      Module id (lower ctx <$> concatMap expandPorts ports) (lower ctx stmt)

data Port = Port
  { _portName :: Ident
  , _portDirection :: Direction
  , _portType :: Type
  } deriving (Eq, Show)

instance HasLower AST.Type Type =>
  HasLower AST.Port Port where
    lower ctx (AST.Port id dir t) = Port id dir (lower ctx t)

-- | only ground types are used
-- and types have explicit widths
data Type
  = Unsigned Int
  | Signed Int
  | Clock
  -- | Vector Type Int
  -- | Bundle [Field]
  -- return type of statements
  | Unit
  deriving (Eq, Show)

flipped :: AST.Orientation -> Direction -> Direction
flipped Direct  Input  = Input
flipped Direct  Output = Output
flipped Flipped Input  = Output
flipped Flipped Output = Input

expandPorts :: AST.Port -> [AST.Port]
expandPorts (AST.Port id dir (AST.Vector t len)) = concatMap expandPorts $ expandVec 0 t
  where expandVec :: Int -> AST.Type -> [AST.Port]
        expandVec n t
          | n >= len = []
          | otherwise = AST.Port (id ++ '$':show n) dir t
                      : expandVec (n + 1) t

  -- where expandVec :: Int -> AST.Type -> [AST.Port] -> [AST.Port]
  --       expandVec n t e
  --         | n >= len = e
  --         | otherwise = expandVec (n + 1) t
  --           (expandPorts (AST.Port (id ++ '$':show n) dir t) ++ e)

expandPorts (AST.Port id dir (AST.Bundle flds)) = concat $ expandField <$> flds
  where expandField :: AST.Field -> [AST.Port]
        expandField (AST.Field orient fid nt) = expandPorts
          (AST.Port (id ++ '$':fid) (flipped orient dir) nt)

expandPorts p = [p]

instance HasLower AST.Type Type where
  lower ctx (AST.Unsigned (Just w)) = Unsigned w
  lower ctx (AST.Signed (Just w))   = Signed w
  lower ctx AST.Clock               = Clock
  lower _ (AST.Vector _ _) = error "Ports should have expanded first"
  lower _ (AST.Bundle _) = error "Ports should have expanded first"
  lower _ _ = error "Type width should be inferred by now"

-- | compared to FIRRTL AST there is no
-- conditional & partial connect statement
data Statement
  = Block [Statement]
  -- | Cond Expr Statement Statement
  | Connect Expr Expr
  | Empty
  | Instance Ident Ident
  | Invalid Expr
  | Memory Mem
  | Node Ident Expr
  -- | Partial Expr Expr
  | Print Expr Expr String [Expr]
  | Reg Ident Type (Maybe Expr) (Maybe Reset)
  | Stop Expr Expr Int
  | Wire Ident Type
  deriving (Eq, Show)

instance HasLower AST.Statement Statement where
  lower ctx (AST.Block [stmt]) = lower ctx stmt
  lower ctx (AST.Block stmts)  = Block (lower ctx <$> stmts)

  -- expand wires, regs and nodes
  lower ctx (AST.Wire id w) = case w of
    (AST.Vector t len) -> Block $ lower ctx <$> expandVec 0 t
      where expandVec :: Int -> AST.Type -> [AST.Statement]
            expandVec n t
              | n >= len = []
              | otherwise = AST.Wire (id ++ '$':show n) t : expandVec (n + 1) t

    (AST.Bundle flds) -> Block $ lower ctx . expandField <$> flds
      where expandField :: AST.Field -> AST.Statement
            expandField (AST.Field orient fid nt) = AST.Wire (id ++ '$':fid) nt

    _ -> Wire id (lower ctx w)

  -- simple transformations
  lower _ AST.Empty                = Empty
  lower _ (AST.Instance id1 id2)   = Instance id1 id2
  lower _ (AST.Print e1 e2 msg es) = Print e1 e2 msg es
  lower _ (AST.Stop e1 e2 ec)      = Stop e1 e2 ec
  lower ctx _ = undefined
