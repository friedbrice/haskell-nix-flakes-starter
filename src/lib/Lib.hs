module Lib
    ( printRuntimeEnvironment
    ) where

import Prim

import qualified System.Environment as Env

printKeyVal :: Show a => Int -> String -> a -> IO ()
printKeyVal lvl key val = do
  putStrLn $ indent lvl $ key <> ":"
  putStrLn $ indent (lvl + 1) $ show val
  putStrLn ""
  where
    indent n str = replicate n '\t' <> str

printRuntimeEnvironment :: IO ()
printRuntimeEnvironment = do
  args <- Env.getArgs
  printKeyVal 0 "Program Args" args

  progName <- Env.getProgName
  printKeyVal 0 "Program Name" progName

  execPath <- Env.getExecutablePath
  printKeyVal 0 "Executable Path" execPath

  env <- Env.getEnvironment
  putStrLn "Environment:\n"
  for_ env $ \(name, value) -> do
    printKeyVal 1 name value
