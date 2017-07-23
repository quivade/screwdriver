module Language.FIRRTL.Parser.Declaration where

import           Control.Applicative
import           Text.Parser.Token.Highlight
import           Text.Trifecta

import Language.FIRRTL.Parser.Common
import Language.FIRRTL.Parser.Statement
import Language.FIRRTL.Syntax

circuitDecl :: RTLParser Circuit
circuitDecl =
  Circuit <$> (reserved "circuit" *> token identifier <* symbol ":"
              <?> "circuit identifier")
          <*> indentedBlock1 moduleDecl

moduleDecl :: RTLParser Module
moduleDecl = Module <$> (reserved "module" *> token identifier <* symbol ":"
                        <?> "module identifier")
                    <*> indentedBlock portDecl
                    <*> (Block <$> indentedBlock1 stmt)
         <|> ExtModule <$> (reserved "extmodule" *> token identifier <* symbol ":"
                           <?> "extendal module identifier")
                       <*> indentedBlock portDecl

portDecl :: RTLParser Port
portDecl = flip Port <$> direction
                     <*> (token identifier <* symbol ":" <?> "port identifier")
                     <*> (typeDecl <?> "port type definition")

direction :: RTLParser Direction
direction = reserved "input" *> pure Input
        <|> reserved "output" *> pure Output
        <?> "port direction specifier"
