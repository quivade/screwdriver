{-|
Copyright   : (c) Quivade, 2017
License     : GPL-3
Maintainer  : Jakub Kopański <jakub@quivade.com>
Stability   : experimental
Portability : POSIX

Common definitions for FIRRTL AST.
|-}
module Language.FIRRTL.Syntax.Common
  ( Ident ) where

import Data.Text      (Text)

type Ident = Text
