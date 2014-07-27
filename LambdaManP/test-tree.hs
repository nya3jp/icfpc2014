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
  tinsertDef
  tinsertNDef
  
  expr $ dbug $ tinsert 3 30 $ tempty  
  expr $ dbug $ tinsert 3 3333 $ tinsert 3 30 $ tempty      
  expr $ dbug $ tinsert 13 1313 $ tinsert 3 30 $ tempty        
  expr $ dbug $ tinsert 8 8888 $ tinsert 3 30 $ tempty          


main2 :: IO ()
main2 = do
  writeFile "test-tree.gcc" $ compile progn
