module While where

import Control.Monad (void)
import Control.Monad.Combinators.Expr -- from parser-combinators
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- Expressions:

-- a   ::= x | n | - a | a opa a
-- b   ::= true | false | not b | b opb b | a opr a
-- opa ::= + | - | * | /
-- opb ::= and | or
-- opr ::= > | <

-- Statements:

-- S ::= x := a | skip | S1; S2 | ( S ) | if b then S1 else S2 | while b do S

-- AST

-- | Boolean expression
data BExpr
  = BoolConst Bool
  | Not BExpr
  | BBinary BBinOp BExpr BExpr
  | RBinary RBinOp AExpr AExpr
  deriving Show

-- | Binary boolean operator
data BBinOp
  = And
  | Or
  deriving Show

-- | Relational operator
data RBinOp
  = Greater
  | Less
  deriving Show

-- | Arithmetic expression
data AExpr
  = Var String
  | IntConst Integer
  | Neg AExpr
  | ABinary ABinOp AExpr AExpr
  deriving Show

-- | Arithmetic operator
data ABinOp
  = Add
  | Subtract
  | Multiply
  | Divide
  deriving Show

-- | Statement
data Stmt
  = Seq [Stmt]
  | Assign String AExpr
  | If BExpr Stmt Stmt
  | While BExpr Stmt
  | Skip
  deriving Show

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

-- | Parses an 'Integer'
integer :: Parser Integer
integer = lexeme L.decimal

-- | Parses a semicolon
semi :: Parser String
semi = symbol ";"

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

rws :: [String]
rws =
  [ "if", "then", "else", "while"
  , "do", "skip", "true", "false"
  , "not", "and", "or"
  ]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x =
      if x `elem` rws
      then fail $ "keyword " ++ show x ++ " cannot be an identifier"
      else return x

-- Parser

whileParser :: Parser Stmt
whileParser = between sc eof stmt

stmt :: Parser Stmt
stmt = f <$> stmt' `sepBy1` semi
  where
    f [s] = s
    f ss  = Seq ss

stmt' :: Parser Stmt
stmt' =
      ifStmt
  <|> whileStmt
  <|> skipStmt
  <|> assignStmt
  <|> parens stmt

ifStmt :: Parser Stmt
ifStmt = do
  rword "if"
  cond <- bExpr
  rword "then"
  stmt1 <- stmt
  rword "else"
  stmt2 <- stmt
  return $ If cond stmt1 stmt2

whileStmt :: Parser Stmt
whileStmt = do
  rword "while"
  cond <- bExpr
  rword "do"
  stmt1 <- stmt
  return $ While cond stmt1

assignStmt :: Parser Stmt
assignStmt = do
  var <- identifier
  void $ symbol ":="
  expr <- aExpr
  return $ Assign var expr

skipStmt :: Parser Stmt
skipStmt = Skip <$ rword "skip"

aExpr :: Parser AExpr
aExpr = makeExprParser aTerm aOperators

bExpr :: Parser BExpr
bExpr = makeExprParser bTerm bOperators

aOperators :: [[Operator Parser AExpr]]
aOperators =
  [ [ Prefix (Neg <$ symbol "-") ]
  , [ InfixL (ABinary Multiply <$ symbol "*")
    , InfixL (ABinary Divide   <$ symbol "/") ]
  , [ InfixL (ABinary Add      <$ symbol "+")
    , InfixL (ABinary Subtract <$ symbol "-") ]
  ]

bOperators :: [[Operator Parser BExpr]]
bOperators =
  [ [ Prefix (Not <$ rword "not") ]
  , [ InfixL (BBinary And <$ rword "and")
    , InfixL (BBinary Or  <$ rword "or")
    ]
  ]

aTerm :: Parser AExpr
aTerm = parens aExpr
  <|> Var <$> identifier
  <|> IntConst <$> integer

bTerm :: Parser BExpr
bTerm = parens bExpr
  <|> (BoolConst True  <$ rword "true")
  <|> (BoolConst False <$ rword "false")
  <|> rExpr

rExpr :: Parser BExpr
rExpr = do
  a1 <- aExpr
  op <- relation
  a2 <- aExpr
  return $ RBinary op a1 a2

relation :: Parser RBinOp
relation =
  (Greater <$ symbol ">") <|>
  (Less    <$ symbol "<")
