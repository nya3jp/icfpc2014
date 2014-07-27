module Main where

import System.Environment

import Desugar
import DSL
import Tree



main :: IO ()
main = do
  args <- getArgs
  case args of
    ["debug"] -> do
      mapM_ putStrLn $ compile' progn
    _ -> main2
    
progn :: LMan ()
progn = do
  expr $ dbug $ tempty  
    
main2 :: IO ()
main2 = do
  writeFile "tree-test.gcc" $ compile progn
