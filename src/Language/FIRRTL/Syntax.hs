{-|
Copyright   : (c) Quivade, 2017
License     : GPL-3
Maintainer  : Jakub Kopański <jakub@quivade.com>
Stability   : experimental
Portability : POSIX

This module provides FIRRTL Abstract Syntax Tree.
|-}
module Language.FIRRTL.Syntax
  ( Expr (..)
  , ExprF
  , Type (..)
  , TypeF
  , Ident
  ) where

import Language.FIRRTL.Syntax.Common
import Language.FIRRTL.Syntax.Expr
import Language.FIRRTL.Syntax.Types

-- data Circuit = Circuit
--   { _circuitTop :: Ident
--   , _circuitModules :: [Module]
--   }

-- data Module
--   = Module
--     { _moduleName :: Ident
--     , _modulePorts :: [Port]
--     , _module :: Statement
--     }
--   | ExtModule
--     { _extModuleName :: Ident
--     , _extModulePorts :: [Port]
--     }

-- data Direction = Input | Output
--   deriving (Eq, Show)

-- data Port = Port
--   { _portName :: Ident
--   , _portDirection :: Direction
--   , _portType :: Type
--   } deriving (Eq, Show)

-- data ReadUnderWrite
--   = Old | New | Undefined
--   deriving (Eq, Show)

-- data Statement
--   = Block [Statement]
--   | Cond Expr Statement Statement
--   | Connect Expr Expr
--   | Empty
--   | Instance Ident Ident
--   | Invalid Expr
--   | Memory Mem
--   | Node Ident Expr
--   | Partial Expr Expr
--   | Print Expr Expr String [Expr]
--   | Reg Ident Type (Maybe Expr) (Maybe Reset)
--   | Stop Expr Expr Int
--   | Wire Ident Type
--   deriving Eq

-- data Mem = Mem
--   { _memName :: Ident
--   , _memData :: Type
--   , _memDepth :: Int
--   , _memWriteLatency :: Int
--   , _memReadLatency :: Int
--   , _memReaders :: [Ident]
--   , _memWriters :: [Ident]
--   , _memReadWriters :: [Ident]
--   , _memReadUnderWrite :: ReadUnderWrite
--   } deriving (Eq, Show)

-- emptyMem :: Mem
-- emptyMem = Mem "" Unit 0 0 0 [] [] [] Undefined

-- data Reset = Reset Expr Expr
--   deriving (Eq, Show)

-- Pretty printing
-- instance Pretty Module => Pretty Circuit where
--   pretty c = pretty ("circuit" :: Text)
--          <+> (pretty . _circuitTop) c <> colon <> hardline
--          <> indent 2 (vsep (pretty <$> _circuitModules c))
-- instance Show Circuit where
--   show = prettyToString

-- instance Pretty Module where
--   pretty (Module id ports stmt) =
--     pretty ("module" :: Text) <+> pretty id <> colon <> hardline
--   pretty (ExtModule id ports) =
--     pretty ("module" :: Text) <+> pretty id <> colon <> hardline
-- instance Show Module where
--   show = prettyToString

-- instance Pretty Expr where
--   pretty (Lit i) = pretty i
--   -- pretty (G g) = pretty g
--   pretty (Valid cond v) = pretty ("validif" :: Text)
--                        <> parens (pretty cond <> comma <+> pretty v)
--   pretty (Mux cond a b) = pretty ("mux" :: Text)
--                        <> parens (hsep (punctuate comma (pretty <$> [cond, a, b])))
--   pretty (Ref id) = pretty id
--   pretty (SubField n f) = pretty n <> dot <> pretty f
--   pretty (SubAccess n a) = pretty n <> brackets (pretty a)
--   pretty (Op prim) = undefined
-- instance Show Expr where
--   show = prettyToString

-- instance Pretty Expr => Pretty Statement where
--   pretty (Connect lhs rhs) = pretty lhs <+> "<=" <+> pretty rhs
-- instance Show Statement where
--   show = prettyToString
