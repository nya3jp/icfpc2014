{-# LANGUAGE FlexibleInstances, GADTs, RecursiveDo, ScopedTypeVariables #-}

import qualified Data.Map as M
import Data.Maybe
import Data.Char
import Debug.Trace
import System.Environment

import Desugar
import DSL

-----

progn :: LMan ()
progn = do
  (step :: Expr (Int -> Int -> (Int,Int))) <- fun2 $ \i j ->
    cons i 1
  expr $ cons (0 :: Expr Int) step



main :: IO ()
main = do
  args <- getArgs
  case args of
    ["debug"] -> do
      putStrLn $ compile' progn
    _ -> do
      putStrLn $ compile progn
