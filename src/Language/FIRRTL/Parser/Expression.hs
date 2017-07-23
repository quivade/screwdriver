module Language.FIRRTL.Parser.Expression
  ( expr ) where

import           Control.Applicative
import qualified Data.Char as C
import           Data.List (foldl', intersperse)
import           Text.Parser.Expression
import           Text.Parser.Token hiding (binary, octal, hexadecimal)
import           Text.Trifecta hiding (octal, hexadecimal, integer)

import Language.FIRRTL.Parser.Common
import Language.FIRRTL.Syntax

expr :: (Monad m, TokenParsing m) => m Expr
expr = term >>= sub

sub :: (Monad m, TokenParsing m) => Expr -> m Expr
sub e = do
  msub <- optional (dot <|> symbolic '[')
  case msub of
    (Just c) ->
      sub =<< if c == '.'
        then SubField e <$> ref
        else SubAccess e <$> (expr <* symbolic ']')
    Nothing -> pure e

term :: (Monad m, TokenParsing m) => m Expr
term = (Lit <$> (fromInteger <$> natural) <?> "natural integer")
   <|> (G <$> constant <?> "ground value")
   <|> ref
   <|> mux
   <|> valid
   <|> op
   <|> parens expr

-- unfortunately Text.Parser.Token does not export this function
-- https://hackage.haskell.org/package/parsers-0.12.4/docs/src/Text-Parser-Token.html#number
number :: TokenParsing m => Integer -> m Char -> m Integer
number base baseDigit =
  foldl' (\x d -> base*x + toInteger (C.digitToInt d)) 0 <$> some baseDigit

binary, octal, hexadecimal :: TokenParsing m => m Integer
binary = number 2 (oneOf "01")
octal = number 8 (oneOf "01234567")
hexadecimal = number 16 (satisfy C.isHexDigit)

data S = S | U

sign :: (Monad m, TokenParsing m) => m S
sign = reserved "UInt" *> pure U
   <|> reserved "SInt" *> pure S

size :: (Monad m, TokenParsing m) => m (Maybe Int)
size = optional (angles $ fromInteger <$> natural)

neg :: (Monad m, TokenParsing m) => m Integer -> m Integer
neg p = do
  min <- optional (symbolic '-')
  case min of
    Just _ -> negate <$> p
    Nothing -> p

value :: (Monad m, TokenParsing m) => m Int
value = parens (fromInteger <$>
    (integer
 <|> between (char '"') (char '"') (char 'b' *> neg binary
                                <|> char 'o' *> neg octal
                                <|> char 'h' *> neg hexadecimal)))

constant :: (Monad m, TokenParsing m) => m Ground
constant = do
  s <- sign
  msize <- size
  val <- value
  let calc = ceiling $ logBase 2.0 (fromIntegral val)

  dcons <- case s of
    U -> if val < 0
            then unexpected "Expected unsigned integer"
            else pure UInt
    _ -> pure SInt

  -- check size
  size <- case msize of
    (Just s) -> if s < calc
                   then unexpected $ "Provided bit width is smaller than the "
                    ++ "number of bits required to represent specified value"
                  else pure s
    Nothing -> pure calc
  pure $ dcons size val

ref :: (Monad m, TokenParsing m) => m Expr
ref = Ref <$> identifier <?> "Reference"

mux :: (Monad m, TokenParsing m) => m Expr
mux = do
  reserved "mux" <?> "Multiplexor keyword"
  args <- parens $ commaSep expr
  if length args /= 3
     then unexpected "Expected 3 arguments to multiplexor"
     else let [cond, a, b] = args
           in pure $ Mux cond a b

valid :: (Monad m, TokenParsing m) => m Expr
valid = do
  reserved "validif" <?> "Conditional validity"
  args <- parens $ commaSep expr
  if length args /= 2
     then unexpected "Expected 2 arguments to conditional validity"
     else let [cond, arg] = args
           in pure $ Valid cond arg

-- generic function parser?
-- op :: (Monad m, TokenParsing m) => String -> [m a] -> m Prim
-- op keyword args = do
--   reserved keyword <?> ("Primitive operation: " ++ keyword)
--   symbolic '('
--   a <- sequence (intersperse comma args)
--   symbolic ')'
--   pure $ Op a

twoExpr :: (Monad m, TokenParsing m)
        => String -> (Expr -> Expr -> Prim) -> m Prim
twoExpr keyword cons = cons
  <$> (reserved keyword *> symbolic '(' *> expr <* comma)
  <*> (expr <* symbolic ')')
  <?> (keyword ++ " primitive operation")

oneExpr :: (Monad m, TokenParsing m)
        => String -> (Expr -> Prim) -> m Prim
oneExpr keyword cons = cons
  <$> (reserved keyword *> parens expr)
  <?> (keyword ++ " primitive operation")

intExpr :: (Monad m, TokenParsing m)
        => String -> (Expr -> Int -> Prim) -> m Prim
intExpr keyword cons = cons
  <$> (reserved keyword *> symbolic '(' *> expr <* comma)
  <*> (fromInteger <$> natural <* symbolic ')')
  <?> (keyword ++ " primitive operation")

twoIntExpr :: (Monad m, TokenParsing m)
        => String -> (Expr -> Int -> Int -> Prim) -> m Prim
twoIntExpr keyword cons = cons
  <$> (reserved keyword *> symbolic '(' *> expr <* comma)
  <*> (fromInteger <$> natural <* comma)
  <*> (fromInteger <$> natural <* symbolic ')')
  <?> (keyword ++ " primitive operation")

op :: (Monad m, TokenParsing m) => m Expr
op = Op
  <$> (twoExpr "add" Add
   <|> twoExpr "sub" Sub
   <|> twoExpr "mul" Mul
   <|> twoExpr "div" Div
   <|> twoExpr "mod" Mod
   <|> twoExpr "lt"  Lt
   <|> twoExpr "leq" Leq
   <|> twoExpr "gt"  Gt
   <|> twoExpr "geq" Geq
   <|> twoExpr "eq"  Eq
   <|> twoExpr "neq" Neq
   <|> twoExpr "dshl" DShl
   <|> twoExpr "dshr" DShr
   <|> twoExpr "and" And
   <|> twoExpr "or"  Or
   <|> twoExpr "cat" Cat
   <|> oneExpr "asUInt" AsUnsigned
   <|> oneExpr "asSInt" AsSigned
   <|> oneExpr "asClock" AsClock
   <|> oneExpr "cvt" Cvt
   <|> oneExpr "neg" Neg
   <|> oneExpr "not" Not
   <|> oneExpr "xor" Xor
   <|> oneExpr "andr" AndR
   <|> oneExpr "orr" OrR
   <|> oneExpr "xorr" XorR
   <|> intExpr "pad" Pad
   <|> intExpr "shl" Shl
   <|> intExpr "shr" Shr
   <|> intExpr "head" Head
   <|> intExpr "tail" Tail
   <|> twoIntExpr "bits" Bits)
