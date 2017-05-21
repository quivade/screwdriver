module Language.Verilog.Parser.Module where

import Control.Applicative ((<$>), (<*>), (<|>))
import Data.Text (Text)
import Text.Parser.Combinators
import Text.Parser.Token.Highlight
import Text.Trifecta

import Language.Verilog.Parser.Identifiers
import Language.Verilog.Syntax.Module

moduleDecl :: (Monad m, TokenParsing m) => m Module
moduleDecl =
  Module <$> (reserved "module"  *> token identifier <?> "module identifier")
         <* semi
         <*> many alphaNum
         <* whiteSpace
         <* reserved "endmodule"

moduleParamDecl :: (Monad m, TokenParsing m) => m ModuleParam
moduleParamDecl = undefined

moduleParamType :: (Monad m, TokenParsing m) => m ParamType
moduleParamType = reserved "integer"  *> pure Integ
              <|> reserved "real"     *> pure Real
              <|> reserved "realtime" *> pure Realtime
              <|> reserved "time"     *> pure Time

-- moduleSign :: (Monad m, TokenParsing m) => m ParamSign
-- moduleSign = reserved "signed" *> pure Signed

range :: (Monad m, TokenParsing m) => m Range
range = brackets $ Range <$> integer <* token (char ':') <*> integer
