{-# LANGUAGE FlexibleInstances, GADTs, RecursiveDo, ScopedTypeVariables, RecordWildCards, RankNTypes, ImpredicativeTypes, NoMonoLocalBinds #-}

import Data.Maybe
import System.Environment
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

bfs :: Expr (Mat Int) -> Expr (Queue (Int, (Int, Int))) -> Expr Int -> Expr Int
(bfs, bfsDef) = def3 "bfs" $ \bd q target -> comp $
  with2 0 undef $ \out qq ->
  with5 undef (cons 0 1) (cons 0 (-1)) (cons 1 0) (cons (-1) 0) $ \cell v1 v2 v3 v4 -> do
    while (lnot $ isEmptyQueue q) $ do
      qq ~= dequeue q
      q ~= cdr qq

      let dep = car $ car qq
          pos = cdr $ car qq

      cell ~= peekMat (car pos) (cdr pos) bd

      trace (c 10002, dep, pos, cell)

      cond (cell .== target)
        ( do
             out ~= dep
             q ~= emptyQueue
        )
        ( cond((cell .== 0) ||| (cell .== 6)) (e $ c 0) $ do
             bd ~= pokeMat (car pos) (cdr pos) 0 bd
             q ~= enqueue (cons (dep + 1) $ vadd pos v1) q
             q ~= enqueue (cons (dep + 1) $ vadd pos v2) q
             q ~= enqueue (cons (dep + 1) $ vadd pos v3) q
             q ~= enqueue (cons (dep + 1) $ vadd pos v4) q
        )

    e out

toQueue :: Expr [a] -> Expr (Queue (Int, a))
(toQueue, toQueueDef) = def1 "toQueue" $ \xs -> comp $
  with emptyQueue $ \q -> do
    while (lnot $ isNull xs) $ do
      q ~= enqueue (cons (c 0) $ lhead xs) q
      xs ~= ltail xs
    e q

nearestGhost :: Expr (Mat Int) -> Expr [Pos] -> Expr Pos -> Expr Int
(nearestGhost, nearestGhostDef) = def3 "nearestGhost" $ \bd gs lpos -> comp $ do
  bd ~= pokeMat (car lpos) (cdr lpos) 5 bd
  e $ bfs bd (toQueue gs) 5

-- [1] if NearestGhost<3 then FromGhost+
-- [1] if MaxJunctionSafety>3 then FromGhost-
-- [2] if NearestEdGhost>99 then ToPowerDot+
-- [2] if NearestEdGhost<99 then ToEdGhost+
-- [2] if GhostDensity<1.5 and NearestPowerDot<5 then FromPowerDot+
-- [3] if Constant>0 then ToCenterofDots+

data X = X
type Pos = (Int, Int)

type World = ([[Int]], (ManState, ([GhostState], FruitState)))
type ManState = (X, (Pos, X))
type GhostState = (X, (Pos, X))
type FruitState = Int

type AIState = Mat Int

mapGhostPos :: Expr [GhostState] -> Expr [Pos]
(mapGhostPos, mapGhostPosDef) = def1 "mapGhostPos" $ \xs -> do
  ite (isNull xs) lnull $ lcons (cadr $ lhead xs) (mapGhostPos $ ltail xs)

-- lmap :: (Expr a -> Expr b) -> Expr [a] -> Expr [b]
-- (lmap, lmapDef) = def2 "lmap" $ \f xs -> do
--   ite (isNull xs) lnull $ lcons (f $ lhead xs) $ lmap f (ltail xs)

step :: Expr AIState -> Expr World -> Expr (AIState, Int)
(step, stepDef) = def2 "step" $ \bd world -> comp $ do
  let lmanPos = cadr $ cadr world
      ghostPoss = mapGhostPos $ caddr world
      -- pos = cons 11 12

  -- trace lmanPos
  -- trace ghostPoss
  -- trace $ toQueue ghostPoss

  trace (c 10001, nearestGhost bd ghostPoss lmanPos)

  -- for 0 1000 $ \i -> do
  --   e $ peekMat 0 0 bd

  with (peek 0 bd) $ \row -> do
    for 0 1000 $ \i -> do
      e $ peek 0 row

  e $ cons bd (c 0)

arrLength :: Expr (Array a) -> Expr Int
arrLength = car

matSize :: Expr (Mat a) -> Expr (Int, Int)
matSize m = cons (arrLength $ peek 0 m) (arrLength m)

initialize :: Expr World -> Expr X -> Expr AIState
(initialize, initializeDef) = def2 "initialize" $ \w _ -> comp $ do
  with4 (toMat (car w)) undef undef undef $ \mat sz w h -> do
    sz ~= matSize mat
    w ~= car sz
    h ~= cdr sz

    for 0 h $ \y ->
      for 0 w $ \x -> e $
        ite (peekMat x y mat .<= c 3) (c 0) $
          comp $ mat ~= pokeMat x y 1 mat
    e $ mat

progn :: LMan ()
progn = do
  libDef

  stepDef
  initializeDef

  bfsDef
  mapGhostPosDef
  nearestGhostDef
  toQueueDef

  rtn $ cons (initialize (Var (-1) 0) (Var (-1) 1)) (Closure "step")
  -- emitComp $ trace $ toQueue $ list [c 1, c 2]

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
