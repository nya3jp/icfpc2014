{-# LANGUAGE FlexibleInstances, GADTs, RecursiveDo, ScopedTypeVariables, RecordWildCards, RankNTypes, ImpredicativeTypes, NoMonoLocalBinds #-}

import qualified Data.Map as M
import Data.Maybe
import Data.Char
import Debug.Trace
import System.Environment
import Unsafe.Coerce
import Control.Applicative

import DSL
import Lib

import Prelude hiding (even, odd)

-----

data State = Int

type Map = [[Int]]
type LMState = Int
type Ghost = (Int, (Int, Int), Int) -- (vital, loc, direction)

-- vital: 0: standard:, 1: fright mode, 2: invisible
-- direction: 0: up, 1: right, 2: down, 3: left

type WorldState = (Map, (Int, (Int, Int)))

vadd :: Expr (Int, Int) -> Expr (Int, Int) -> Expr (Int, Int)
vadd a b = cons (car a + car b) (cdr a + cdr b)

(bfs :: Expr [[Int]] -> Expr (Queue (Int, Int)) -> Expr (Int, Int) -> Expr (Int, Int) -> Expr (Int, Int), bfsDef) = def4 "paint" $ \bd q initPos out -> comp $ do
  q ~= enqueue initPos emptyQueue

  while (lnot $ isEmptyQueue q) $ comp $ do
    let pos = car $ dequeue q
    let cell = getMat (car pos) (cdr pos) bd

    debug $ cons (i 10002) (cons pos cell)

    e $ ite ((cell .== 2) ||| (cell .== 3))
          (comp $ do
              debugn 10005
              out ~= pos
              q ~= emptyQueue
              debug $ cons (i 10006) $ cons q out
          )
          $ comp $ do
              e $ ite ((cell .== 0) ||| (cell .== 6)) (0 :: Expr Int) $ comp $ do
                bd ~= setMat (car pos) (cdr pos) 0 bd
                -- record path
                q ~= enqueue (vadd pos (cons 0    1   )) q
                q ~= enqueue (vadd pos (cons 0    (-1))) q
                q ~= enqueue (vadd pos (cons 1    0   )) q
                q ~= enqueue (vadd pos (cons (-1) 0   )) q

              q ~= cdr (dequeue q)

  debugn 10006
  debug out
  e out

type X = Int
type World = ([[Int]], (ManState, (X, X)))
type ManState = (X, (Pos, X))
type AIState = X
type Pos = (Int, Int)

(step, stepDef) = def2 "step" $ \st world -> comp $ do
  let bd = car world
      -- pos = cadr $ cadr world
      pos = cons 11 12

  debug pos
  debugn 123
  debug $ (cons (i 777) $ bfs bd emptyQueue pos (cons 0 0))

  return ()

{-
progn :: LMan ()
progn = do
  libDef

  bfsDef
  stepDef

  expr $ cons (0 :: Expr Int) (Closure "step")
-}

progn :: LMan ()
progn = do
  libDef

  expr $ comp $ do
    debug $ lreverse $ list [1, 2, 3, 4, 5 :: Expr Int]
    debugn 1234

    e $ with 123 $ \i -> comp $ do
      debugn i
      debugn $ i * 2
      i ~= i * i
      debugn i

  expr $ do
    with (enqueue 1 emptyQueue :: Expr (Queue Int)) $ \q1 ->
      with (enqueue 2 q1) $ \q2 ->
      with (enqueue 3 q2) $ \q3 ->
      with (dequeue q3) $ \r4 ->
      with (dequeue $ cdr r4) $ \r5 ->
      with (dequeue $ cdr r5) $ \r6 -> comp $ do
        debug q2
        debug q3
        debug r4
        debug r5
        debug r6

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["debug"] -> do
      mapM_ putStrLn $ compile' progn
    _ -> do
      putStrLn $ compile progn
