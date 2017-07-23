module Language.FIRRTL.Parser.Statement
  ( stmt
  -- individual statements
  , connect
  , empty
  , inst
  , invalid
  , memory
  , node
  , partial
  , print
  , register
  , wire
  , stop
  ) where

import           Prelude hiding (print)
import           Control.Applicative hiding (empty)
import           Control.Monad.State.Strict
import qualified Data.Char as C
import           Data.List (foldl', intersperse)
import           Text.Trifecta

import Language.FIRRTL.Parser.Common
import Language.FIRRTL.Parser.Declaration
import Language.FIRRTL.Parser.Expression
import Language.FIRRTL.Syntax

stmt :: RTLParser Statement
stmt = conditional
   <|> connect
   <|> empty
   <|> inst
   <|> invalid
   <|> memory
   <|> node
   <|> partial
   <|> print
   <|> register
   <|> stop
   <|> wire

conditional :: RTLParser Statement
conditional = Cond
          <$> (reserved "when" *> expr <* colon)
          <*> (Block <$> indentedBlock1 stmt)
          <*> option Empty (reserved "else" *> elseBranch)
  where elseBranch :: RTLParser Statement
        elseBranch = Block <$> (symbolic ':' *> indentedBlock1 stmt)
                 <|> conditional

connect :: RTLParser Statement
connect = Connect
  <$> (expr <* reserved "<=")
  <*> expr

partial :: RTLParser Statement
partial = Partial
  <$> (expr <* reserved "<-")
  <*> expr

empty :: RTLParser Statement
empty = reserved "skip" *> pure Empty <?> "empty statement"

inst :: RTLParser Statement
inst = Instance
   <$> (reserved "inst" *> token identifier)
   <*> (reserved "of" *> token identifier)

invalid :: RTLParser Statement
invalid = Invalid <$> expr <* reserved "is" <* reserved "invalid"

node :: RTLParser Statement
node = Node
   <$> (reserved "node" *> token identifier <* symbol "=")
   <*> expr

print :: RTLParser Statement
print = Print
    <$> (reserved "print" *> symbolic '('
     *> expr <?> "clock signal")
    <*> (comma *> expr <?> "print condition signal")
    <*> (comma *> stringLiteral <?> "print format srting")
    <*> option [] (comma *> commaSep1 expr)
    <* symbolic ')'

register :: RTLParser Statement
register = Reg
       <$> (reserved "reg" *> token identifier <* colon)
       <*> typeDecl <* skipOptional comma
       <*> optional (expr <?> "clock signal")
       <*> optional
        (reserved "with" *> colon *> indentedBlockS reset <?> "reset signal and value")

reset :: RTLParser Reset
reset = (reserved "reset" *> reserved "=>")
     *> parens (Reset <$> expr <* comma <*> expr)

stop :: RTLParser Statement
stop = Stop
   <$> (reserved "stop" *> symbolic '('
    *> expr <?> "clock signal")
   <*> (comma *> expr <?> "halt condition signal")
   <*> (comma *> (fromIntegral <$> integer) <* symbolic ')' <?> "exit code")

wire :: RTLParser Statement
wire = Wire
   <$> (reserved "wire" *> token identifier <* colon)
   <*> typeDecl <?> "wire"

memory :: RTLParser Statement
memory = do
  reserved "mem"
  ident <- token identifier
  colon
  modify (\s -> s { memoryState = Mem ident Unit 0 0 0 [] [] [] Undefined})
  indentedBlock1 memItem
  Memory <$> gets memoryState
    where memItem = memDataType
                <|> memDepth
                <|> memWriteLatency
                <|> memReadLatency
                <|> memReader
                <|> memWriter
                <|> memReadWriter
                <|> memReadUnderWrite

memParser :: String -> RTLParser a -> RTLParser a
memParser f p = reserved f *> reserved "=>" *> p

memDataType :: RTLParser ()
memDataType = do
  mst <- gets memoryState
  t <- memParser "data-type" typeDecl
  modify (\s -> s { memoryState = mst { _memData = t }})

memDepth:: RTLParser ()
memDepth = do
  mst <- gets memoryState
  t <- fromInteger <$> memParser "depth" natural
  modify (\s -> s { memoryState =  mst { _memDepth = t }})

memWriteLatency:: RTLParser ()
memWriteLatency = do
  mst <- gets memoryState
  t <- fromInteger <$> memParser "write-latency" natural
  modify (\s -> s { memoryState = mst { _memWriteLatency = t }})

memReadLatency :: RTLParser ()
memReadLatency = do
  mst <- gets memoryState
  t <- fromInteger <$> memParser "read-latency" natural
  modify (\s -> s { memoryState = mst { _memReadLatency = t }})

memReader:: RTLParser ()
memReader = do
  mst <- gets memoryState
  t <- memParser "reader" (token identifier)
  modify (\s -> s { memoryState = mst { _memReaders = t : _memReaders mst }})

memWriter :: RTLParser ()
memWriter = do
  mst <- gets memoryState
  t <- memParser "writer" (token identifier)
  modify (\s -> s { memoryState = mst { _memWriters = t : _memWriters mst }})

memReadWriter :: RTLParser ()
memReadWriter = do
  mst <- gets memoryState
  t <- memParser "readwriter" (token identifier)
  modify (\s -> s { memoryState = mst { _memReadWriters = t : _memReadWriters mst }})

memReadUnderWrite :: RTLParser ()
memReadUnderWrite = do
  mst <- gets memoryState
  t <- memParser "read-under-write" ruw
  modify (\s -> s { memoryState = mst { _memReadUnderWrite = t }})

ruw :: (Monad m, TokenParsing m) => m ReadUnderWrite
ruw = reserved "new" *> pure New
  <|> reserved "old" *> pure Old
  <|> reserved "undefined" *> pure Undefined
