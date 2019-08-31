module Main (main) where

import Control.Monad.IO.Class (liftIO)
import System.Console.Haskeline (InputT, runInputT, getInputLine, outputStrLn, defaultSettings)
import qualified REPL

main :: IO ()
main = runInputT defaultSettings loop

loop :: InputT IO ()
loop = do
  input <- getInputLine "Repl> "
  case input of
    Nothing -> outputStrLn "Goodbye."
    Just s -> (liftIO $ REPL.process s) >> loop
