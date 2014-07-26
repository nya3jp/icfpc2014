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

data State = Int

cadr = car . cdr
caddr = car . cdr . cdr
cdddr = cdr . cdr . cdr

type WorldState = (Int, (Int, (Int, Int)))

progn :: LMan ()
progn = do
  -- Lib {..} <- lib
  -- nth :: forall a. Expr ([a] -> Int -> a) <- nth'

  let mm = Var 0 0

  rec
    (nth :: Expr ([Int] -> Int -> Int)) <- fun2 $ \xs i ->
      ite (i .== 0)
      (lhead xs)
      (call2 nth (ltail xs) (i - 1))

    (length :: Expr ([Int] -> Int)) <- fun1 $ \xs ->
      ite (atom xs) 0 (1 + call1 length (ltail xs))

  step <- fun2 $ \(state :: Expr State) (world :: Expr WorldState) -> comp $ do
    e $ dbug world
    e $ dbug $ car world
    e $ dbug $ cadr world
    e $ dbug $ caddr world
    e $ dbug $ cdddr world

  expr $ cons (0 :: Expr Int) step
  return ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["debug"] -> do
      putStrLn $ compile' progn
    _ -> do
      putStrLn $ compile progn
