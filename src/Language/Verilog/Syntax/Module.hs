module Language.Verilog.Syntax.Module where

import Data.Text (Text)

import Language.Verilog.Syntax.Number (Sign (..))

data Module = Module
  { _moduleName  :: String
  , _modulePorts :: [Port]
  } deriving (Eq, Show)

data ModuleItem = P Port

data Direction = Inout | Input | Output
  deriving (Eq, Show)

data Port = Port
  { _portDir   :: Direction
  , _portNet   :: Net
  , _portSign  :: Sign
  , _portRange :: Maybe Range
  , _portIdent :: String
  } deriving (Eq, Show)

data Net
  = Supply0
  | Supply1
  | Tri
  | TriAnd
  | TriOr
  | Tri0
  | Tri1
  | Unwire
  | Wire
  | WAnd
  | WOr
  deriving (Eq, Show)

data Range = Range
  { _msb :: Integer
  , _lsb :: Integer
  } deriving (Eq, Show)

data ParamType
  = Integ
  | Real
  | Realtime
  | Time
  deriving (Eq, Show)

data ModuleParam = ModuleParam
  { _paramName  :: String
  , _paramType  :: Maybe ParamType
  -- , _paramSign  :: Maybe ParamSign
  , _paramRange :: Maybe Range
  , _paramExpr  :: String
  } deriving (Eq, Show)
