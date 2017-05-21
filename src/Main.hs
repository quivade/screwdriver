module Main where

import Control.Monad
import Text.Parsec.String (Parser)
import qualified Text.Parsec as P
import Language.Verilog.Parser

-- |
-- Simplified type for parse function
parse :: Parser a -> P.SourceName -> String -> Either P.ParseError a
parse = P.parse

try :: Parser a -> Parser a
try = P.try

runParser :: Parser a -> String -> Either P.ParseError a
runParser m = parse m ""

main :: IO ()
main =
    forever $
    do putStr ">"
       a <- getLine
       print $ runParser unsigned a
