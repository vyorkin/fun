module REPL (process) where

import Text.Megaparsec (runParser)
import Parser (parser)
import Eval (eval)
import Pretty (ppExpr)

process :: String -> IO ()
process line = do
  let res = runParser parser "<stdin>" line
  case res of
    Left err -> print err
    Right expr -> run expr
  where
    run e = case eval e of
      Nothing -> putStrLn "Cannot evaluate"
      Just r -> putStrLn $ ppExpr r
