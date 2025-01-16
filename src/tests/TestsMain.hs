module TestsMain where

import Lib (printRuntimeEnvironment)

import System.IO.Silently (silence)

main :: IO ()
main = do
  silence printRuntimeEnvironment
  putStrLn "Ran printRuntimeEnvironment and it didn't crash."
