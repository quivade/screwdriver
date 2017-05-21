module Language.Verilog.Syntax.Module where

import Data.Text (Text)

data Module = Module
  { moduleName :: String
  , moduleText :: String
  } deriving (Eq, Show)

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
