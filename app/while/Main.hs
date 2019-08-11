module Main (main) where

import Text.Megaparsec (parseTest)
import While

main :: IO ()
main = do
  input <- getContents
  parseTest whileParser input
