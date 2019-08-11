{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE LambdaCase #-}

{-| Description: A toy parser combinator library -}
module NanoParsec where

import Control.Applicative
import Control.Monad (MonadPlus, mplus, mzero)
import Data.Char

-- | A parser is a function that takes an input stream of
-- characters and yields a parse tree by applying the parser
-- logic over sections of the character stream (lexems) to
-- build up a composite data structure for the AST.
newtype Parser a = Parser { parse :: String -> [(a, String)] }

-- | Runs the parser and yields a value of type @a@ (AST) for
-- the parsed expression or fails with a parse error.
runParser :: Parser a -> String -> a
runParser m s = case parse m s of
  [(r, [])] -> r
  [(_, cs)] -> error $ "Parser did not consume entire stream: " ++ cs
  _         -> error "Parser error"

-- | Advances the parser by extracting a
-- single character from the parser.
item :: Parser Char
item = Parser $ \case
  []     -> []
  (c:cs) -> [(c, cs)]

-- Since the parser operation yields a list of tuples, composing
-- a second parser function simply maps itself over the
-- resulting list and concat's the resulting nested list of lists.
bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s ->
  concatMap (\(a, s') -> parse (f a) s') $ parse p s

-- | Injects a single pure value as the result,
-- without reading from the parse stream.
unit :: a -> Parser a
unit a = Parser (\s -> [(a, s)])

instance Functor Parser where
  fmap f (Parser cs) = Parser $ \s ->
    [(f a, b) | (a, b) <- cs s]

instance Applicative Parser where
  pure = return
  (Parser cs1) <*> (Parser cs2) = Parser $ \s ->
    [ (f a, s2)
    | (f, s1) <- cs1 s
    , (a, s2) <- cs2 s1
    ]

instance Monad Parser where
  return = unit
  (>>=)  = bind

instance Alternative Parser where
 empty = mzero
 (<|>) = option

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

-- | Applies two parser functions over the
-- same stream and concatenates the result.
combine :: Parser a -> Parser a -> Parser a
combine p q = Parser $ \s -> parse p s ++ parse q s

-- | Halts reading a stream and returns the empty stream.
failure :: Parser a
failure = Parser $ const []

option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s ->
  case parse p s of
    [] -> parse q s
    r  -> r

-- | Checks whether the current character in
-- the stream matches a given predicate.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \c ->
  if p c
  then unit c
  else failure

-- Higher order behavior

oneOf :: [Char] -> Parser Char
oneOf = satisfy . flip elem

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = p `chainl1` op <|> return a

-- | Parses one or more occurences of @p@, separated by @op@ and
-- returns a value obtainted by a recursing until failure on
-- the left hand side of the stream.
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= chain
  where
    chain a = rest a <|> return  a
    rest a = do
      f <- op
      b <- p
      chain $ f a b

char :: Char -> Parser Char
char c = satisfy (c ==)

natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

string :: String -> Parser String
string [] = return []
string (c:cs) = do
  char c
  string cs
  return $ c:cs

spaces :: Parser String
spaces = many $ oneOf " \n\r"

reserved :: String -> Parser String
reserved s = token (string s)

token :: Parser a -> Parser a
token p = do
  a <- p
  spaces
  return a

digit :: Parser Char
digit = satisfy isDigit

number :: Parser Int
number = do
  s  <- string "-" <|> return []
  cs <- some digit
  return $ read (s ++ cs)

parens :: Parser a -> Parser a
parens m = do
  reserved "("
  n <- m
  reserved ")"
  return n
