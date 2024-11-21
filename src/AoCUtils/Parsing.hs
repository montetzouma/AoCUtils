module AoCUtils.Parsing
  ( Parser 
  , intParser 
  , signedIntParser )
where

import qualified Data.Void                  as V
import qualified Text.Megaparsec            as TM
import qualified Text.Megaparsec.Char       as TMC
import qualified Text.Megaparsec.Char.Lexer as TMCL


type Parser = TM.Parsec V.Void String


intParser :: Parser Int
intParser = lexeme TMCL.decimal
  where
    lexeme  = TMCL.lexeme TMC.space

signedIntParser :: Parser Int 
signedIntParser = TMCL.signed TMC.space intParser
