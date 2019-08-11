module Parser where

import Control.Monad (void)
import Control.Monad.Combinators.Expr -- from parser-combinators
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- | Space consumer
sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment  = L.skipLineComment ""
    blockComment = L.skipBlockComment "" ""
