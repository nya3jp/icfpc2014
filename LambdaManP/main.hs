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

bfs :: Expr (Mat Int) -> Expr (Int, Int) -> Expr (Int, Int)
(bfs, bfsDef) = def2 "paint" $ \bd initPos -> with (enqueue initPos emptyQueue) $ \q -> with (cons 0 0) $ \out -> comp $ do

  while (lnot $ isEmptyQueue q) $ comp $ do
    let pos = car $ dequeue q
    let cell = peekMat (car pos) (cdr pos) bd

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
                bd ~= pokeMat (car pos) (cdr pos) 0 bd
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
  let bd = toMat $ car world
      -- pos = cadr $ cadr world
      pos = cons 11 12

  debug bd
  debug $ cons (i 777) $ bfs bd pos

progn :: LMan ()
progn = do
  libDef

  bfsDef
  stepDef

  rtn $ cons (0 :: Expr Int) (Closure "step")

{-
progn :: LMan ()
progn = do
  libDef

  -- cexpr $ do
  --   cwith (mkArray (list $ map fromIntegral [0..9 :: Int] :: Expr [Int])) $ \arr -> do
  --     debug arr

  --     for 0 10 $ \i -> do
  --       debug $ cons i $ peek i arr

  --     for 0 10 $ \i -> do
  --       arr ~= poke i (peek i arr * peek i arr) arr

  --     debug arr

  --     for 0 10 $ \i -> do
  --       debug $ cons i $ peek i arr

  -- cexpr $ do
  --   debug $ lreverse $ list [1, 2, 3, 4, 5 :: Expr Int]
  --   debugn 1234

  --   cwith 123 $ \i -> do
  --     debugn i
  --     debugn $ i * 2
  --     i ~= i * i
  --     debugn i

  --   cwith 0 $ \i -> do
  --     while (i .< 10) $ comp $ do
  --       debugn i
  --       i ~= i + 1

  -- expr $ do
  --   with (enqueue 1 emptyQueue :: Expr (Queue Int)) $ \q1 ->
  --     with (enqueue 2 q1) $ \q2 ->
  --     with (enqueue 3 q2) $ \q3 ->
  --     with (dequeue q3) $ \r4 ->
  --     with (dequeue $ cdr r4) $ \r5 ->
  --     with (dequeue $ cdr r5) $ \r6 -> comp $ do
  --       debug q2
  --       debug q3
  --       debug r4
  --       debug r5
  --       debug r6
-}

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["debug"] -> do
      mapM_ putStrLn $ compile' progn
    _ -> do
      putStrLn $ compile progn
