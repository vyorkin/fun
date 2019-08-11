{-# LANGUAGE LambdaCase #-}

{-| Description: A simple calculator -}
module Calculator where

import Control.Applicative
import NanoParsec

-- Our (BNF) grammar:

-- number = [ ”-” ] digit { digit }.
-- digit
-- = ”0” | ”1” | ... | ”8” | ”9”.
-- expr
-- = term { addop term }.
-- term
-- = factor { mulop factor }.
-- factor = ”(” expr ”)” | number.
-- addop
-- = ”+” | ”-”.
-- mulop
-- = ”*”.

data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Lit Int
  deriving Show

eval :: Expr -> Int
eval = \case
  Add a b -> eval a + eval b
  Mul a b -> eval a * eval b
  Sub a b -> eval a - eval b
  Lit n   -> n

int :: Parser Expr
int = do
  n <- number
  return $ Lit n

expr :: Parser Expr
expr = term `chainl1` addOp

addOp :: Parser (Expr -> Expr -> Expr)
addOp = infixOp "+" Add <|> infixOp "-" Sub

mulOp :: Parser (Expr -> Expr -> Expr)
mulOp = infixOp "*" Mul

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = reserved x >> return f

term :: Parser Expr
term = factor `chainl1` mulOp

factor :: Parser Expr
factor = int <|> parens expr

run :: String -> Expr
run = runParser expr
