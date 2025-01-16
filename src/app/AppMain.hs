module AppMain where

import Lib

main :: IO ()
main = do
  putStrLn "About to run printRuntimeEnvironment..."
  printRuntimeEnvironment
