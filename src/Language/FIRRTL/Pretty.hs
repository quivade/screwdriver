module Language.FIRRTL.Pretty where

import qualified Data.Text.Lazy            as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text

prettyToString :: Pretty a => a -> String
prettyToString a = T.unpack
  $ renderLazy
  $ layoutPretty defaultLayoutOptions
  $ pretty a
