module Language.FIRRTL.Syntax where

type Ident = String

data Circuit = Circtuit
  { _circuitTop :: Ident
  , _circuitModules :: [Module]
  } deriving Show

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

data Direction = Input | Output
  deriving (Eq, Show)

data Port = Port
  { _portName :: Ident
  , _portDirection :: Direction
  } deriving (Eq, Show)

data Ground
  = SInt Int Int
  | UInt Int Int
  | Clk
  deriving (Eq, Show)

data Type
  = Unsigned Int
  | Signed Int
  | Clock
  | Fipped Type
  | Vector Type Int
  | Bundle [(Ident,Type)]
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

-- data Expression = Expression
--   { _gender :: Gender
--   , _width  :: Int
--   , _term   :: Term
--   } deriving (Eq, Show)

data Expr
  -- | A ground type literal value
  = Lit Ground
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
  | Field Expr Ident
  -- | A SubIndex expression refers, by index, to a subelement of an expression
  -- with a Vector type.
  | Index Expr Int
  -- | A SubAccess expression dynamically refers to a subelement of a
  -- vector-typed expresion using a calculated index.
  | Access Expr Expr
  -- | All fundamental operations on ground types
  | Op Prim
  deriving (Eq, Show)

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
  | Mem { _memData :: Type
        , _memDepth :: Int
        , _memWriteLatency :: Int
        , _memReadLatency :: Int
        , _memReaders :: [Ident]
        , _memWriters :: [Ident]
        , _memReadWriters :: [Ident]
        , _memReadUnderWrite :: Maybe ReadUnderWrite
        }
  | Node Ident Expr
  | Partial Expr Expr
  | Print Expr Expr String [Expr]
  | Reg Type Ident Expr Expr Expr
  | Stop Int Expr Expr
  | Wire Type Ident
  deriving (Eq, Show)
