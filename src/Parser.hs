module Parser where

import Control.Monad.Combinators.Expr -- from parser-combinators
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Syntax (Expr(..))

type Parser = Parsec Void String

-- Lexer

-- | Space consumer
sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment  = L.skipLineComment "--"
    blockComment = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- | Parses something between parenthesis
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parses a semicolon
semi :: Parser String
semi = symbol ";"

reserved :: String -> Parser ()
reserved w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

keywords :: [String]
keywords =
  [ "true"
  , "false"
  , "0"
  , "succ"
  , "pred"
  , "iszero"
  , "if", "then", "else"
  ]

operators :: [[Operator Parser Expr]]
operators =
  [ [ Prefix (Succ <$ symbol "succ")
    , Prefix (Pred <$ symbol "pred")
    , Prefix (IsZero <$ symbol "iszero")
    ]
  ]

true, false, zero :: Parser Expr
true  = reserved "true"  >> return Tr
false = reserved "false" >> return Fl
zero  = reserved "0"     >> return Zero

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reserved "then"
  tr <- expr
  reserved "else"
  fl <- expr
  return $ If cond tr fl

expr :: Parser Expr
expr = makeExprParser factor operators

factor :: Parser Expr
factor = true
     <|> false
     <|> zero
     <|> ifthen
     <|> parens expr

parser :: Parser Expr
parser = between sc eof expr
