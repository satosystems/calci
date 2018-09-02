module Calculator.Parser where

import Data.Functor.Identity (Identity)
import Text.Parsec
  ( ParseError
  , (<|>)
  , char
  , eof
  , parse
  , try
  )
import Text.Parsec.Expr
  ( Assoc(AssocLeft)
  , Operator(Infix)
  , buildExpressionParser
  )
import Text.Parsec.String (Parser)

import Calculator.Lexer
  ( float
  , identifier
  , integer
  , parens
  , reservedOp
  , whiteSpace
  )
import Calculator.Syntax
  ( Expr(Calc, Double, Subst, Var)
  , Op(Divide, Minus, Plus, Times)
  )

binary :: String -> Op -> Assoc -> Operator String () Identity Expr
binary s f = Infix (reservedOp s >> return (Calc f))

table :: [[Operator String () Identity Expr]]
table =
  [ [ binary "*" Times AssocLeft, binary "/" Divide AssocLeft ]
  , [ binary "+" Plus AssocLeft, binary "-" Minus AssocLeft ]
  ]

factor :: Parser Expr
factor = try int
  <|> try double
  <|> try subst
  <|> try variable
  <|> parens expr

expr :: Parser Expr
expr = buildExpressionParser table factor

int :: Parser Expr
int = Double . fromIntegral <$> integer

double :: Parser Expr
double = Double <$> float

variable :: Parser Expr
variable = Var <$> identifier

subst :: Parser Expr
subst = do
  name <- identifier
  whiteSpace
  _ <- char '='
  whiteSpace
  Subst (Var name) <$> expr

contents :: Parser a -> Parser a
contents p = do
  whiteSpace
  r <- p
  eof
  return r

topLevel :: Parser Expr
topLevel = do
  formula <- expr
  _ <- eof
  return formula

parseToplevel :: FilePath -> String -> Either ParseError Expr
parseToplevel = parse (contents topLevel)

