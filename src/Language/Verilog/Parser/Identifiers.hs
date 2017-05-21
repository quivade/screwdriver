module Language.Verilog.Parser.Identifiers where

import           Control.Applicative         ((<$>), (<*>), (<|>))
import qualified Data.HashSet                as HashSet
import           Text.Parser.Token.Highlight
import           Text.Trifecta

verilogIdents :: TokenParsing m => IdentifierStyle m
verilogIdents = IdentifierStyle
  { _styleName = "identifier"
  , _styleStart = letter <|> char '_'
  , _styleLetter = alphaNum <|> oneOf "_$"
  , _styleReserved = HashSet.fromList vlog2005ReservedIdents
  , _styleHighlight = Identifier
  , _styleReservedHighlight = ReservedIdentifier
  }

vlog2005ReservedIdents :: [String]
vlog2005ReservedIdents =
  ["always","and","assign","automatic","begin","buf","bufif0","bufif1"
  ,"case","casex","casez","cell","cmos","config"
  ,"deassign","default","defparam","design","disable"
  ,"edge","else","end","endcase","endconfig","endfunction","endgenerate"
  ,"endmodule","endprimitive","endspecify","endtable","endtask","event"
  ,"for","force","forever","fork","function"
  ,"generate","genvar"
  ,"highz0","highz1"
  ,"if","ifnone","incdir","include","initial","inout","input","instance","integer"
  ,"join"
  ,"large","liblist","library","localparam"
  ,"macromodule","medium","module"
  ,"nand","negedge","nmos","nor","noshowcancelled","not","notif0","notif1"
  ,"or","output"
  ,"parameter","pmos","posedge","primitive","pull0","pull1","pulldown","pullup"
  ,"pulsestyle_onevent","pulsestyle_ondetect"
  ,"rcmos","real","realtime","reg","release","repeat","rnmos","rpmos"
  ,"rtran","rtranif0","rtranif1"
  ,"scalared","showcancelled","signed","small","specify","specparam"
  ,"strong0","strong1","supply0","supply1"
  ,"table","task","time","tran","tranif0","tranif1","tri","tri0","tri1"
  ,"triand","trior","trireg"
  ,"unsigned","use","uwire"
  ,"vectored"
  ,"wait","wand","weak0","weak1","while","wire","wor"
  ,"xnor","xor"
  ]

identifier :: (Monad m, TokenParsing m) => m String
identifier = ident verilogIdents

reserved :: (Monad m, TokenParsing m) => String -> m ()
reserved = reserve verilogIdents
