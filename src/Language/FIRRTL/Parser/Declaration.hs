module Language.FIRRTL.Parser.Declaration where

import           Control.Applicative
import           Text.Parser.Token.Highlight
import           Text.Trifecta

import Language.FIRRTL.Parser.Common
import Language.FIRRTL.Syntax

circuitDecl :: (Monad m, TokenParsing m) => m Circuit
circuitDecl =
  Circuit <$> (reserved "circuit" *> token identifier <* symbol ":"
              <?> "circuit identifier")
          <*> some moduleDecl

moduleDecl :: (Monad m, TokenParsing m) => m Module
moduleDecl = Module <$> (reserved "module" *> token identifier <* symbol ":"
                        <?> "module identifier")
                    <*> many portDecl
                    <*> statement
         <|> ExtModule <$> (reserved "extmodule" *> token identifier <* symbol ":"
                           <?> "extendal module identifier")
                       <*> many portDecl

portDecl :: (Monad m, TokenParsing m) => m Port
portDecl = flip Port <$> direction
                     <*> (token identifier <* symbol ":" <?> "port identifier")
                     <*> (typeDecl <?> "port type definition")

direction :: (Monad m, TokenParsing m) => m Direction
direction = reserved "input" *> pure Input
        <|> reserved "output" *> pure Output
        <?> "port direction specifier"

basicType :: (Monad m, TokenParsing m) => m Type
basicType = Unsigned <$> (reserved "UInt" *> (fromInteger <$> angles natural))
        <|> Signed <$> (reserved "SInt" *> (fromInteger <$> angles natural))
        <|> reserved "Clock" *> pure Clock
        <|> reserved "flip" *> typeDecl
        <|> Bundle <$> braces (sepBy1 fieldDecl (symbol ","))

fieldDecl :: (Monad m, TokenParsing m) => m Field
fieldDecl = Field <$> (option Direct (reserved "flip" *> pure Flipped)
                      <?> "orientation specifier")
                  <*> (token identifier <* symbol ":" <?> "field identifier")
                  <*> (typeDecl <?> "field type")

typeDecl :: (Monad m, TokenParsing m) => m Type
typeDecl = do
  t <- basicType
  size <- optional (fromInteger <$> brackets natural)
  return $ case size of
    Just n -> Vector t n
    Nothing -> t

statement :: (Monad m, TokenParsing m) => m Statement
statement = pure Empty
