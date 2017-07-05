module Language.Verilog.Parser.Module where

import Control.Applicative ((<$>), (<*>), (<|>))
import Data.Text (Text)
import Text.Parser.Combinators
import Text.Parser.Token.Highlight
import Text.Trifecta

import Language.Verilog.Parser.Identifiers
import Language.Verilog.Syntax.Module
import Language.Verilog.Syntax.Number (Sign (..))

moduleDecl :: (Monad m, TokenParsing m) => m Module
moduleDecl =
  Module <$> (reserved "module" *> token identifier <?> "module identifier")
         <*> (modulePortDecls <?> "list of port delcarations")
         <* semi
         -- <*> many moduleItem
         <* whiteSpace
         <* reserved "endmodule"

moduleItem :: (Monad m, TokenParsing m) => m ModuleItem
moduleItem = P <$> portDecl

modulePortDecls :: (Monad m, TokenParsing m) => m [Port]
modulePortDecls = parens $ portDecl `sepBy` (symbol ",")

portDecl :: (Monad m, TokenParsing m) => m Port
portDecl = Port <$> direction
                <*> option Wire net
                <*> option Unsigned signed
                <*> optional range
                <*> token identifier <?> "port identifier"

direction :: (Monad m, TokenParsing m) => m Direction
direction = reserved "inout" *> pure Inout
        <|> reserved "input" *> pure Input
        <|> reserved "output" *> pure Output
        <?> "direction specifier"

signed :: (Monad m, TokenParsing m) => m Sign
signed = reserved "signed" *> pure Signed

net :: (Monad m, TokenParsing m) => m Net
net = reserved "supply0" *> pure Supply0
  <|> reserved "supply1" *> pure Supply1
  <|> reserved "tri"     *> pure Tri
  <|> reserved "triand"  *> pure TriAnd
  <|> reserved "trior"   *> pure TriOr
  <|> reserved "tri0"    *> pure Tri0
  <|> reserved "tri1"    *> pure Tri1
  <|> reserved "unwire"  *> pure Unwire
  <|> reserved "wire"    *> pure Wire
  <|> reserved "wand"    *> pure WAnd
  <|> reserved "wor"     *> pure WOr
  <?> "net type"

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
