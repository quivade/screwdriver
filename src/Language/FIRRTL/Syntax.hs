{-# LANGUAGE UndecidableInstances #-}
module Language.FIRRTL.Syntax where

import Data.Semigroup ((<>))
import Data.Text      (Text)
import Data.Text.Prettyprint.Doc

import Language.FIRRTL.Pretty

type Ident = Text

data Circuit = Circuit
  { _circuitTop :: Ident
  , _circuitModules :: [Module]
  }

instance Pretty Module => Pretty Circuit where
  pretty c = pretty ("circuit" :: Text)
         <+> (pretty . _circuitTop) c <> colon <> hardline
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

instance Pretty Module where
  pretty (Module id ports stmt) =
    pretty ("module" :: Text) <+> pretty id <> colon <> hardline
  pretty (ExtModule id ports) =
    pretty ("module" :: Text) <+> pretty id <> colon <> hardline

instance Show Module where
  show = prettyToString

data Direction = Input | Output
  deriving (Eq, Show)

data Port = Port
  { _portName :: Ident
  , _portDirection :: Direction
  , _portType :: Type
  } deriving (Eq, Show)

data Ground
  = SInt Int Int
  | UInt Int Int
  | Clk
  deriving Eq

instance Pretty Ground where
  pretty (SInt w v) = pretty ("SInt" :: Text) <> angles (pretty w) <> parens (pretty v)
  pretty (UInt w v) = pretty ("UInt" :: Text) <> angles (pretty w) <> parens (pretty v)
  pretty Clk = pretty ("Clock" :: Text)

instance Show Ground where
  show = prettyToString

data Orientation = Direct | Flipped
  deriving (Eq, Show)

data Field = Field
  { _fieldOrientation :: Orientation
  , _fieldName :: Ident
  , _fieldType :: Type
  } deriving (Eq, Show)

data Type
  = Unsigned (Maybe Int)
  | Signed (Maybe Int)
  | Clock
  | Vector Type Int
  | Bundle [Field]
  -- return type of statements
  | Unit
  deriving (Eq, Show)

data Prim
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Mod Expr Expr
  | Lt  Expr Expr
  | Leq Expr Expr
  | Gt  Expr Expr
  | Geq Expr Expr
  | Eq  Expr Expr
  | Neq Expr Expr
  | Pad Expr Int
  | AsUnsigned Expr
  | AsSigned Expr
  | AsClock Expr
  | Shl Expr Int
  | Shr Expr Int
  | DShl Expr Expr
  | DShr Expr Expr
  | Cvt Expr
  | Neg Expr
  | Not Expr
  | And Expr Expr
  | Or Expr Expr
  | Xor Expr
  | AndR Expr
  | OrR Expr
  | XorR Expr
  | Cat Expr Expr
  | Bits Expr Int Int
  | Head Expr Int
  | Tail Expr Int
  deriving (Eq, Show)

data Expr
  -- | natural integer
  = Lit Int
  -- | A ground type literal value
  | G Ground
  -- | A conditionally valid expression is expressed as an input expression
  -- guarded with an unsigned single bit valid signal.
  -- It outputs the input expression when the valid signal is high,
  -- otherwise the result is undefined.
  | Valid Expr Expr
  -- | A multiplexor outputs one of two input expressions depending on
  -- the value of an unsigned single bit selection signal.
  | Mux Expr Expr Expr
  -- | A name that refers to a previously declared circuit component,
  -- port, node, wire, register, instance or memory.
  | Ref Ident
  -- | A SubField expression refers to a subelement of an expression with
  -- bundle type.
  | SubField Expr Ident
  -- | A SubIndex expression refers, by index, to a subelement of an expression
  -- with a Vector type.
  -- It is indistinguichable from SubAccess since natural int is an expression.
  -- | SubIndex Expr Expr
  -- | A SubAccess expression dynamically refers to a subelement of a
  -- vector-typed expresion using a calculated index.
  | SubAccess Expr Expr
  -- | All fundamental operations on ground types
  | Op Prim
  deriving Eq

instance Pretty Ground => Pretty Expr where
  pretty (Lit i) = pretty i
  pretty (G g) = pretty g
  pretty (Valid cond v) = pretty ("validif" :: Text)
                       <> parens (pretty cond <> comma <+> pretty v)
  pretty (Mux cond a b) = pretty ("mux" :: Text)
                       <> parens (hsep (punctuate comma (pretty <$> [cond, a, b])))
  pretty (Ref id) = pretty id
  pretty (SubField n f) = pretty n <> dot <> pretty f
  pretty (SubAccess n a) = pretty n <> brackets (pretty a)
  pretty (Op prim) = undefined

instance Show Expr where
  show = prettyToString

data ReadUnderWrite
  = Old | New | Undefined
  deriving (Eq, Show)

data Statement
  = Block [Statement]
  | Cond Expr Statement Statement
  | Connect Expr Expr
  | Empty
  | Instance Ident Ident
  | Invalid Expr
  | Memory Mem
  | Node Ident Expr
  | Partial Expr Expr
  | Print Expr Expr String [Expr]
  | Reg Ident Type (Maybe Expr) (Maybe Reset)
  | Stop Expr Expr Int
  | Wire Ident Type
  deriving Eq

instance Pretty Expr => Pretty Statement where
  pretty (Connect lhs rhs) = pretty lhs <+> "<=" <+> pretty rhs

instance Show Statement where
  show = prettyToString

data Mem = Mem
  { _memName :: Ident
  , _memData :: Type
  , _memDepth :: Int
  , _memWriteLatency :: Int
  , _memReadLatency :: Int
  , _memReaders :: [Ident]
  , _memWriters :: [Ident]
  , _memReadWriters :: [Ident]
  , _memReadUnderWrite :: ReadUnderWrite
  } deriving (Eq, Show)

emptyMem :: Mem
emptyMem = Mem "" Unit 0 0 0 [] [] [] Undefined

data Reset = Reset Expr Expr
  deriving (Eq, Show)

data RTLState = RTLState
  { indentStack :: [Int]
  , parenStack  :: [Maybe Int]
  , memoryState :: Mem -- ^ state to parse memory block
  }
