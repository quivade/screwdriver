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
  , Ground (..)
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

import           Data.Semigroup            ((<>))
import qualified Data.Text.Lazy            as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text

import           Language.FIRRTL.Pretty
import           Language.FIRRTL.Syntax ( Direction (Input, Output)
                                        , Orientation (Direct, Flipped)
                                        , Ground (..), Ident, Mem, Reset)
import qualified Language.FIRRTL.Syntax as AST

data Ctx = Ctx

class HasLower s a | a -> s where
  lower :: Ctx -> s -> a

data Circuit = Circuit
  { _circuitTop :: Ident
  , _circuitModules :: [Module]
  }

instance (HasLower AST.Module Module) =>
  HasLower AST.Circuit Circuit where
    lower ctx (AST.Circuit id modules) = Circuit id (lower ctx <$> modules)

instance Pretty Module => Pretty Circuit where
  pretty c = pretty ("circuit" :: T.Text)
         <+> (pretty . T.pack . _circuitTop) c <> colon <> hardline
         <> indent 2 (vsep (pretty <$> _circuitModules c))

instance Show Circuit where
  show = prettyToString

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

instance (HasLower AST.Port Port, HasLower AST.Statement Statement) =>
  HasLower AST.Module Module where
    lower ctx (AST.ExtModule id ports) =
      ExtModule id (lower ctx <$> concatMap expandPorts ports)
    lower ctx (AST.Module id ports stmt) =
      Module id (lower ctx <$> concatMap expandPorts ports) (lower ctx stmt)

instance Pretty Module where
  pretty (Module id ports stmt) =
    pretty ("module" :: T.Text) <+> (pretty . T.pack) id <> colon <> hardline
  pretty (ExtModule id ports) =
    pretty ("module" :: T.Text) <+> (pretty . T.pack) id <> colon <> hardline

instance Show Module where
  show = prettyToString

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

data Expr
  = Lit Int
  | G Ground
  | Valid Expr Expr
  | Mux Expr Expr Expr
  | Ref Ident
  | Op AST.Prim
  deriving Eq

instance Pretty Ground => Pretty Expr where
  pretty (Lit i) = pretty i
  pretty (G g) = pretty g
  pretty (Valid cond v) = pretty ("validif" :: T.Text)
                       <> parens (pretty cond <> comma <+> pretty v)
  pretty (Mux cond a b) = pretty ("mux" :: T.Text)
                       <> parens (hsep (punctuate comma (pretty <$> [cond, a, b])))
  pretty (Ref id) = pretty (T.pack id)
  pretty (Op prim) = undefined

instance Show Expr where
  show = prettyToString

instance HasLower AST.Expr Expr where
  lower _ (AST.Lit n) = Lit n
  lower _ (AST.G g) = G g
  lower ctx (AST.Valid t e) = Valid (lower ctx t) (lower ctx e)
  lower ctx (AST.Mux t a b) = Mux (lower ctx t) (lower ctx a) (lower ctx b)
  lower _ (AST.Ref id) = Ref id
  lower _ (AST.Op p) = Op p

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
  deriving Eq

instance Pretty Expr => Pretty Statement where
  pretty (Connect lhs rhs) = pretty lhs <+> "<=" <+> pretty rhs

instance Show Statement where
  show = prettyToString

expandVec :: (Ident -> AST.Type -> AST.Statement)
          -> Ident -> Int -> AST.Type -> [AST.Statement]
expandVec stmt id n t
  | n < 0 = []
  | otherwise = stmt (id ++ '$':show n) t : expandVec stmt id (n - 1) t

expandField :: (Ident -> AST.Type -> AST.Statement)
            -> Ident -> AST.Field -> AST.Statement
expandField stmt id (AST.Field _ fid t) = stmt (id ++ '$':fid) t

instance HasLower AST.Statement Statement where
  lower ctx (AST.Block [stmt]) = lower ctx stmt
  lower ctx (AST.Block stmts)  = Block (lower ctx <$> stmts)

  lower ctx (AST.Cond test cons alt) = undefined
  -- lower ctx (AST.Connect lhs rhs) = case lhs of
  --   (AST.Ref id) -> Connect lhs rhs
  --   (AST.SubField expr name) -> 
  lower ctx (AST.Invalid expr) = undefined
  lower ctx (AST.Memory mem) = undefined
  lower ctx (AST.Partial lhs rhs) = undefined

  -- expand wires, regs and nodes
  lower ctx (AST.Wire id w) = case w of
    (AST.Vector t len) -> Block $ lower ctx <$> expandVec AST.Wire id (len - 1) t
    (AST.Bundle flds) -> Block $ lower ctx . expandField AST.Wire id <$> flds
    _ -> Wire id (lower ctx w)

  lower ctx (AST.Reg id r mclk mrst) = case r of
    (AST.Vector t len) -> Block $ lower ctx
      <$> expandVec (\id len -> AST.Reg id t mclk mrst) id (len - 1) t
    (AST.Bundle flds) -> Block
      $ lower ctx . expandField
        (\id t -> AST.Reg id t mclk mrst) id <$> flds
    _ -> Reg id (lower ctx r) (lower ctx <$> mclk) mrst

  lower ctx (AST.Node id expr) = Node id (lower ctx expr)

  -- simple transformations
  lower _ AST.Empty                = Empty
  lower _ (AST.Instance id1 id2)   = Instance id1 id2
  lower ctx (AST.Print e1 e2 msg es) = Print (lower ctx e1) (lower ctx e2)
                                        msg (lower ctx <$> es)
  lower ctx (AST.Stop e1 e2 ec)      = Stop (lower ctx e1) (lower ctx e2) ec
