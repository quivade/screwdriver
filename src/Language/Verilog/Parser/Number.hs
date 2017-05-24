module Language.Verilog.Parser.Number where

import Control.Applicative ((<$>), (<*>), (<|>))
import Data.Bits
import qualified Data.Char as C (digitToInt, isHexDigit, isOctDigit)
import           Data.Maybe (fromMaybe)
-- import Data.List (foldl')
import Data.Text (Text)
import Text.Parser.Combinators
import Text.Parser.Token
import Text.Parser.Token.Highlight hiding (Number)
import Text.Trifecta hiding (octDigit, hexDigit)

import Language.Verilog.Syntax.Number
import Language.Verilog.Syntax.Number.Value

size :: (Monad m, TokenParsing m) => m Int
size = token nonZeroUnsigned <?> "number bit size"

signed :: (CharParsing m) => m Sign
signed = option Unsigned (oneOf "sS" *> pure Signed)

hexBase, decBase, octBase, binBase :: (CharParsing m) => m Base
hexBase = oneOf "hH" *> pure HexBase <?> "hexadecimal base"
decBase = oneOf "dD" *> pure DecBase <?> "deciaml base"
octBase = oneOf "oO" *> pure OctBase <?> "octal base"
binBase = oneOf "bB" *> pure BinBase <?> "binary base"

base :: (Monad m, TokenParsing m) => m Base
base = token hexBase <|> token decBase <|> token octBase <|> token binBase

isHighZDigit, isUnknownDigit, isHexDigit, isOctDigit, isBinDigit :: Char -> Bool
isHighZDigit c = c `elem` ("zZ" :: String)
isUnknownDigit c = c `elem` ("xX" :: String)
isHexDigit c = C.isHexDigit c
            || c `elem` ("xXzZ" :: String)
isOctDigit c = C.isOctDigit c
            || c `elem` ("xXzZ" :: String)
isBinDigit c = c `elem` ("01zZxX" :: String)

nonZeroUnsigned :: (Monad m, TokenParsing m) => m Int
nonZeroUnsigned = read <$> do
    d <- nonZeroDecDigit
    optional underscore
    n <- option "" (valueStr decDigit)
    return (d:n)

unsigned :: (Monad m, TokenParsing m) => m Int
unsigned = read . concat <$> sepEndBy1 (some decDigit) underscore

hexDigit, decDigit, nonZeroDecDigit, octDigit, binDigit :: CharParsing m
                                                        => m Char
hexDigit = satisfy isHexDigit <?> "hexadecimal digit"
decDigit = digit
nonZeroDecDigit = oneOf ['1'..'9']  <?> "non zero digit"
octDigit = satisfy isOctDigit <?> "octal digit"
binDigit = satisfy isBinDigit <?> "binary digit"

underscore :: CharParsing m => m Char
underscore = char '_'

valueStr :: (Monad m, TokenParsing m) => m Char -> m String
valueStr baseDigit = concat <$> sepEndBy1 (some baseDigit) underscore

value :: (Monad m, TokenParsing m) => Base -> m String
value HexBase = valueStr hexDigit
value DecBase = valueStr decDigit
value OctBase = valueStr octDigit
value BinBase = valueStr binDigit

number :: (Monad m, TokenParsing m) => m Number
number = do
  msize <- optional size
  char '\'' <?> "base apostrophe"
  sign <- signed
  b <- base
  let pVal = case b of
        DecBase -> decValue . toInteger . read <$> valueStr decDigit
          <|> satisfy isHighZDigit *> pure [HighZ] 
          <|> satisfy isUnknownDigit *> pure [Unknown]
        _       -> concat <$> (fmap (singleValue b) <$> value b)
  val <- token pVal
  let width = fromMaybe (length val) msize
  return (Number sign width (reverse val))

singleValue :: Base -> Char -> [Value]
singleValue HexBase = hexValue
singleValue OctBase = octValue
singleValue BinBase = binValue

decValue :: Integer -> [Value]
decValue 0 = [Zero]
decValue 1 = [One]
decValue i = decValue (i `quot` 2) ++ decValue (i `rem` 2)

hexValue :: Char -> [Value]
hexValue '0' = [Zero, Zero, Zero, Zero]
hexValue '1' = [Zero, Zero, Zero, One ]
hexValue '2' = [Zero, Zero, One , Zero]
hexValue '3' = [Zero, Zero, One , One ]
hexValue '4' = [Zero, One , Zero, Zero]
hexValue '5' = [Zero, One , Zero, One ]
hexValue '6' = [Zero, One , One , Zero]
hexValue '7' = [Zero, One , One , One ]
hexValue '8' = [One , Zero, Zero, Zero]
hexValue '9' = [One , Zero, Zero, One ]
hexValue 'A' = [One , Zero, One , Zero]
hexValue 'B' = [One , Zero, One , One ]
hexValue 'C' = [One , One , Zero, Zero]
hexValue 'D' = [One , One , Zero, One ]
hexValue 'E' = [One , One , One , Zero]
hexValue 'F' = [One , One , One , One ]
hexValue 'Z' = [HighZ, HighZ, HighZ, HighZ]
hexValue  _  = [Unknown, Unknown, Unknown, Unknown]

octValue :: Char -> [Value]
octValue '0' = [Zero, Zero, Zero]
octValue '1' = [Zero, Zero, One ]
octValue '2' = [Zero, One , Zero]
octValue '3' = [Zero, One , One ]
octValue '4' = [One , Zero, Zero]
octValue '5' = [One , Zero, One ]
octValue '6' = [One , One , Zero]
octValue '7' = [One , One , One ]
octValue 'Z' = [HighZ, HighZ, HighZ]
octValue  _  = [Unknown, Unknown, Unknown]

binValue :: Char -> [Value]
binValue '0' = [Zero]
binValue '1' = [One ]
binValue 'Z' = [HighZ]
binValue  _  = [Unknown]

-- value :: Integer -> String -> Value
-- value base s
--   | 'x' `elem` s = Unknown
--   | 'X' `elem` s = Unknown
--   | 'z' `elem` s = HighZ
--   | 'Z' `elem` s = HighZ
--   | otherwise = I $ go 0 base s
--   where go :: Integer -> Integer -> String -> Integer
--         go r _ [] = r
--         go r b (d : ds) = let r' = r * b + (toInteger . C.digitToInt) d
--                            in r' `seq` go r' b ds

-- from Text.Parser.Token
-- number :: TokenParsing m => Integer -> m Char -> m Integer
-- number base baseDigit =
--     foldl' (\x d -> base*x + toInteger (digitToInt d)) 0 <$> some baseDigit

-- hexValue, decValue, octValue, binValue :: (Monad m, TokenParsing m) => m Value
-- hexValue = value 16 <$> valueStr hexDigit
-- decValue = value 10 <$> valueStr decDigit
-- octValue = value  8 <$> valueStr octDigit
-- binValue = value  2 <$> valueStr binDigit

-- number :: (Monad m, TokenParsing m) => m Number
-- number = do
--   size <- optional $ token nonZeroUnsigned
--   sign <- option Unsigned signedBase
--   b    <- token base
--   sval <- token value
--   let width = case size of
--         Just s  -> s
--         Nothing -> floor $ 1 + logBase 2
--   return (Number sign size sval)

-- sign :: CharParsing m => m Sign
-- sign = oneOf "sS" *> pure Signed

-- binValueStr :: (Monad m, TokenParsing m) => m String
-- binValueStr = 

-- binValue :: (Monad m, TokenParsing m) => m Value
-- binValue =

-- verilogValue :: Parser Char -> Parser String
-- verilogValue baseDigit =
--     concat <$> sepEndBy1 (some baseDigit) underscore

-- number :: Parser Number
-- number = do
--   s <- optional nonZeroUnsigned
--   b <- base
--   v <- verilogValue (getDigit b)
--   return $ getNumber s b v

-- getNumber :: Base -> Maybe Integer -> Base -> String -> Number
-- getNumber (Signed b) = getNumber b
-- getNumber HexBase    = Hex
-- getNumber DecBase    = Dec
-- getNumber OctBase    = Oct
-- getNumber BinBase    = Bin

-- getDigit :: Base -> Parser Char
-- getDigit (Signed b) = getDigit b
-- getDigit HexBase    = hexDigit
-- getDigit DecBase    = decDigit
-- getDigit OctBase    = octDigit
-- getDigit BinBase    = binDigit
