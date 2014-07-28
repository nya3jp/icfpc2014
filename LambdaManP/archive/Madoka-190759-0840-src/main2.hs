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
  return ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["debug"] -> do
      putStrLn $ compile' progn
    _ -> do
      putStrLn $ compile progn
