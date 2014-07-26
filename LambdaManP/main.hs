{-# LANGUAGE FlexibleInstances, GADTs, RecursiveDo, ScopedTypeVariables, RecordWildCards, RankNTypes, ImpredicativeTypes #-}

import qualified Data.Map as M
import Data.Maybe
import Data.Char
import Debug.Trace
import System.Environment
import Unsafe.Coerce
import Control.Applicative

import Desugar
import DSL

-----

progn :: LMan ()
progn = do
  -- Lib {..} <- lib
  nth :: forall a. Expr ([a] -> Int -> a) <- nth'

  expr $ with 1 $ \i ->
    dbugn i    `Seq`
    (i ~= (i*2)) `Seq`
    dbugn i

  f :: Expr (Int -> Int) <- fun1 $ \i -> i + 1

  expr $ dbug $ cons (0 :: Expr Int) f

  expr $ dbug (list [1, 3, 4, 5] :: Expr [Int])
  expr $ dbugn $ call2 nth (list [1, 2, 3, 4, 5]) 3
  expr $ dbug (call2 nth (list [list[1]]) 0 :: Expr [Int])
  return ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["debug"] -> do
      putStrLn $ compile' progn
    _ -> do
      putStrLn $ compile progn
