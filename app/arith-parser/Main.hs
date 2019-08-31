module Main (main) where

import Text.Megaparsec (parseTest)
import Parser (parser)

main :: IO ()
main = do
  input <- getContents
  parseTest parser input
