{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
module Language.FIRRTL.Parser.Common where

import           Control.Applicative
import           Control.Monad.State.Strict

import qualified Data.HashSet                as HashSet
import           Data.Maybe                  (isJust, isNothing)
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import           Text.Parser.LookAhead
import           Text.Parser.Token.Highlight
import           Text.Trifecta
import           Text.Trifecta.Delta

import Language.FIRRTL.Syntax

type RTLParser = StateT RTLState RTLInnerParser

newtype RTLInnerParser a = RTLInnerParser { runInnerParser :: Parser a }
  deriving (Applicative, Alternative, Functor, Monad, MonadPlus, Monoid,
            CharParsing, DeltaParsing, LookAheadParsing, TokenParsing)

deriving instance Parsing RTLInnerParser

testParser :: (MonadIO m, Show a) => RTLParser a -> String -> m ()
testParser p = parseTest (runInnerParser (evalStateT p (RTLState [] [] emptyMem)))

firrtlIdents :: TokenParsing m => IdentifierStyle m
firrtlIdents = IdentifierStyle
  { _styleName = "identifier"
  , _styleStart = letter <|> char '_'
  , _styleLetter = alphaNum <|> char '_'
  , _styleReserved = firrtlReserved <> firrtlPrimOps
  , _styleHighlight = Identifier
  , _styleReservedHighlight = ReservedIdentifier
  }

firrtlReserved :: HashSet.HashSet String
firrtlReserved = HashSet.fromList
  [ "Analog", "attach"
  , "circuit", "Clock"
  , "data-type", "depth"
  , "else", "extmodule"
  , "Fixed", "flip"
  , "input", "inst", "invalid", "is"
  , "mem", "module", "mux"
  , "new", "node"
  , "old", "output"
  , "printf"
  , "reader", "readwriter", "read-latency", "read-under-write", "reg"
  , "SInt", "skip", "stop"
  , "undefined", "UInt"
  , "when", "wire", "writer", "write-latency"
  , "validif"
  ]

firrtlPrimOps :: HashSet.HashSet String
firrtlPrimOps = HashSet.fromList
  [ "add"
  , "sub"
  , "mul"
  , "div"
  , "mod"
  , "lt"
  , "leq"
  , "gt"
  , "geq"
  , "eq"
  , "neq"
  , "pad"
  , "asUInt"
  , "asSInt"
  , "asClock"
  , "shl"
  , "shr"
  , "dshl"
  , "dshr"
  , "cvt"
  , "neg"
  , "not"
  , "and"
  , "or"
  , "xor"
  , "andr"
  , "orr"
  , "xorr"
  , "cat"
  , "bits"
  , "head"
  , "tail"
  ]

identifier :: (Monad m, TokenParsing m) => m Ident
identifier = ident firrtlIdents

reserved :: (Monad m, TokenParsing m) => Text -> m ()
reserved = reserveText firrtlIdents

basicType :: (Monad m, TokenParsing m) => m Type
basicType = Unsigned <$> (reserved "UInt"
                      *> (fmap . fmap) fromInteger (optional (angles natural)))
        <|> Signed <$> (reserved "SInt"
                    *> (fmap . fmap) fromInteger (optional (angles natural)))
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

lookAheadMatches :: (Monad m, LookAheadParsing m) => m a -> m Bool
lookAheadMatches p = do match <- lookAhead (optional p)
                        pure $ isJust match

indent :: (Monad m, DeltaParsing m) => m Int
indent = ((+1) . fromIntegral . column) <$> position

lastIndent :: RTLParser Int
lastIndent = do
  rst <- get
  case indentStack rst of
    (x:xs) -> pure x
    _      -> pure 1

indented :: RTLParser a -> RTLParser a
indented p = notEndBlock *> p <* keepTerminator

indentedBlock :: RTLParser a -> RTLParser [a]
indentedBlock p = do
  openBlock
  pushIndent
  res <- many (indented p)
  popIndent
  closeBlock
  pure res

indentedBlock1 :: RTLParser a -> RTLParser [a]
indentedBlock1 p = do
  openBlock
  pushIndent
  res <- some (indented p)
  popIndent
  closeBlock
  pure res

indentedBlockS :: RTLParser a -> RTLParser a
indentedBlockS p = do
  openBlock
  pushIndent
  res <- indented p
  popIndent
  closeBlock
  pure res

pushIndent :: RTLParser ()
pushIndent = do
  pos <- position
  rst <- get
  put (rst { indentStack = (fromIntegral (column pos) + 1) : indentStack rst })

popIndent :: RTLParser ()
popIndent = do
  rst <- get
  case indentStack rst of
    [] -> error "Tried to pop an indentation level where none wos pushed (underflow)."
    (x:xs) -> put (rst { indentStack = xs })

openBlock :: RTLParser ()
openBlock = do symbolic '('
               rst <- get
               put (rst { parenStack = Nothing : parenStack rst })
        <|> do rst <- get
               lvl' <- indent
               let lvl = case parenStack rst of
                     Just lvlOld : _ ->
                       if lvl' <= lvlOld
                          then lvlOld + 1
                          else lvl'
                     [] -> if lvl' == 1 then 2 else lvl'
                     _ -> lvl'
               put (rst { parenStack = Just lvl : parenStack rst })
        <?> "start of a block"

closeBlock :: RTLParser ()
closeBlock = do
  rst <- get
  ps <- case parenStack rst of
          [] -> eof >> pure []
          Nothing : xs -> symbolic ')' *> pure xs <?> "end of a block"
          Just lvl : xs -> (do i <- indent
                               if i >= lvl
                                  then fail "not end of a block"
                                  else pure xs)
                           <|> (do notOpenParens
                                   eof
                                   pure [])
  put (rst { parenStack = ps })

notEndBlock :: RTLParser ()
notEndBlock = do
  rst <- get
  case parenStack rst of
    Just lvl : xs -> do i <- indent
                        isParen <- lookAheadMatches (char ')')
                        when (i < lvl || isParen) (fail "end of a block")
    _ -> pure ()

keepTerminator :: RTLParser ()
keepTerminator = do symbolic ','
                    pure ()
             <|> do c <- indent
                    l <- lastIndent
                    unless (c <= l) (fail "not a terminator")
             <|> do isParen <- lookAheadMatches (char ')')
                    unless isParen (fail "not a terminator")
             <|> lookAhead eof

notOpenParens :: RTLParser ()
notOpenParens = do rst <- get
                   when (hasNothing $ parenStack rst) (fail "end of input")
                     where hasNothing :: [Maybe a] -> Bool
                           hasNothing = any isNothing
