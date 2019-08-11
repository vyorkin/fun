module Main (main) where

import Control.Monad (forever)
import Calculator

main :: IO ()
main = forever $ do
  putStr "> "
  a <- getLine
  print $ eval $ run a
