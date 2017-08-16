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
  , Expr (..)
  , Ground (..)
  , Ident
  , Orientation (..)
  , Reset

  -- AST overrides
  , Circuit (..)
  , Module (..)
  , Port (..)
  , Statement (..)
  , expandPorts
  , lower
  ) where

import           Data.Semigroup            ((<>))
import qualified Data.Text.Lazy            as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text

import           Language.FIRRTL.Pretty
import           Language.FIRRTL.Syntax ( Direction (Input, Output)
                                        , Expr (..)
                                        , Ground (..), Ident
                                        , Mem, Orientation (Direct, Flipped)
                                        , Reset)
import qualified Language.FIRRTL.Syntax as AST

class HasLower s a | a -> s where
  lower :: s -> [a]

data Circuit = Circuit
  { _circuitTop :: Ident
  , _circuitModules :: [Module]
  }

-- instance (HasLower AST.Module Module) =>
--   HasLower AST.Circuit Circuit where
--     lower (AST.Circuit id modules) = [Circuit id (concatMap lower modules)]

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

-- instance (HasLower AST.Port Port, HasLower AST.Statement Statement) =>
--   HasLower AST.Module Module where
--     lower (AST.ExtModule id ports) =
--       [ExtModule id (concatMap lower ports)]
--     lower (AST.Module id ports stmt) =
--       [Module id (concatMap lower ports) (lower stmt)]

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
    lower (AST.Port id dir t) =
      zipWith3 Port (lowerNames t id) (lowerDirection t dir) (lower t)

lowerNames :: AST.Type -> Ident -> [Ident]
lowerNames (AST.Vector t len) id =
  let ids = (\n -> id ++ '$': show n) <$> take len (iterate (+1) 0)
   in concatMap (lowerNames t) ids
lowerNames (AST.Bundle flds) id =
  let ids = (\f -> id ++ '$':AST._fieldName f) <$> flds
      types = AST._fieldType <$> flds
   in concat $ zipWith lowerNames types ids
lowerNames _ id = [id]

lowerDirection :: AST.Type -> Direction -> [Direction]
lowerDirection (AST.Bundle flds) dir =
  let dirs = (`flipped` dir) . AST._fieldOrientation <$> flds
      types = AST._fieldType <$> flds
   in concat $ zipWith lowerDirection types dirs
lowerDirection (AST.Vector _ len) dir = replicate len dir
lowerDirection _ dir = [dir]

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

instance HasLower AST.Type Type where
  lower (AST.Unsigned (Just size)) = [Unsigned size]
  lower (AST.Signed (Just size)) = [Signed size]
  lower AST.Clock = [Clock]
  lower (AST.Vector t len) = concat $ replicate len (lower t)
  lower (AST.Bundle flds) = concatMap (lower . AST._fieldType) flds
  lower AST.Unit = [Unit]
  lower _ = error "Infer type width first"

-- data Expr
--   = Lit Int
--   | G Ground
--   | Valid Expr Expr
--   | Mux Expr Expr Expr
--   | Ref Ident
--   | Op AST.Prim
--   deriving Eq

-- instance Pretty Ground => Pretty Expr where
--   pretty (Lit i) = pretty i
--   pretty (G g) = pretty g
--   pretty (Valid cond v) = pretty ("validif" :: T.Text)
--                        <> parens (pretty cond <> comma <+> pretty v)
--   pretty (Mux cond a b) = pretty ("mux" :: T.Text)
--                        <> parens (hsep (punctuate comma (pretty <$> [cond, a, b])))
--   pretty (Ref id) = pretty (T.pack id)
--   pretty (Op prim) = undefined

-- instance Show Expr where
--   show = prettyToString

-- instance HasLower AST.Expr Expr where
--   lower (AST.SubField n f) = 
--   lower (AST.Lit n) = [Lit n]
--   lower (AST.G g) = [G g]
--   lower (AST.Valid t e) = [Valid (lower ctx t) (lower ctx e)]
--   lower (AST.Mux t a b) = [Mux (lower ctx t) (lower ctx a) (lower ctx b)]
--   lower (AST.Ref id) = [Ref id]
--   lower (AST.Op p) = [Op p]

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
  -- lower (AST.Block stmts)  = concatMap lower stmts
  lower (AST.Block stmts)  = [Block $ concatMap lower stmts]

--   lower ctx (AST.Cond test cons alt) = undefined
--   -- lower ctx (AST.Connect lhs rhs) = case lhs of
--   --   (AST.Ref id) -> Connect lhs rhs
--   --   (AST.SubField expr name) -> 
--   lower ctx (AST.Invalid expr) = undefined
--   lower ctx (AST.Memory mem) = undefined
  lower (AST.Partial lhs rhs) = case lhs of
    (Ref id) -> [Connect lhs rhs]
    (SubAccess expr acc) = case acc of
      (Lit n) -> appendName <$> [Connect expr rhs]

      (G ground) = case ground of
        (UInt size val) -> appendName <$> [Connect expr rhs]
        _ -> error "Subaccess expression index must be an Unsigned Integer"

    (SubField expr field) -> appendField <$> concatMap lower [Connect expr rhs]
    _ -> error "Invalid left hand side of partial connect statement"
    

--   -- expand wires, regs and nodes
--   lower ctx (AST.Wire id w) = case w of
--     (AST.Vector t len) -> Block $ lower ctx <$> expandVec AST.Wire id (len - 1) t
--     (AST.Bundle flds) -> Block $ lower ctx . expandField AST.Wire id <$> flds
--     _ -> Wire id (lower ctx w)

--   lower ctx (AST.Reg id r mclk mrst) = case r of
--     (AST.Vector t len) -> Block $ lower ctx
--       <$> expandVec (\id len -> AST.Reg id t mclk mrst) id (len - 1) t
--     (AST.Bundle flds) -> Block
--       $ lower ctx . expandField
--         (\id t -> AST.Reg id t mclk mrst) id <$> flds
--     _ -> Reg id (lower ctx r) (lower ctx <$> mclk) mrst

  -- simple transformations
  lower stmt = [stmt]
  -- lower (AST.Node id expr)       = [Node id expr]
  -- lower AST.Empty                = [Empty]
  -- lower (AST.Instance id1 id2)   = [Instance id1 id2]
  -- lower (AST.Print e1 e2 msg es) = [Print e1 e2 msg es]
  -- lower (AST.Stop e1 e2 ec)      = [Stop e1 e2 ec]
