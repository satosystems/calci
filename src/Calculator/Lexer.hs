module Calculator.Lexer
  ( float
  , identifier
  , integer
  , parens
  , reservedOp
  , whiteSpace
  ) where

import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser emptyDef

integer :: Parser Integer
integer = Tok.integer lexer

float :: Parser Double
float = Tok.float lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

