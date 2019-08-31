module REPL (process) where

import Text.Megaparsec (runParser)
import Parser (parser)
import Eval (eval)

process :: String -> IO ()
process line = do
  let res = runParser parser "<stdin>" line
  case res of
    Left err -> print err
    Right expr -> print $ eval expr
