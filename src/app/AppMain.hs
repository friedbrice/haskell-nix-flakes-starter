module AppMain where

import Prim

import Lib

main :: IO ()
main = do
  putStrLn "About to run printRuntimeEnvironment..."
  printRuntimeEnvironment
