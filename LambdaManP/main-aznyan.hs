{-# LANGUAGE FlexibleInstances, GADTs, RecursiveDo, ScopedTypeVariables, RecordWildCards, RankNTypes, ImpredicativeTypes #-}

import qualified Data.Map as M
import Data.Maybe
import Data.Char
import Debug.Trace
import System.Environment
import Unsafe.Coerce

import Desugar
import DSL

-----
type X = Int
type World = ([[Int]], (ManState, (X, X)))
type ManState = (X, (Pos, X))
type AIState = X
type Pos = (Int, Int)

(tileValue :: Expr Int -> Expr Int, tileValueDef) = def1 "tileView" $ \i ->
  ite (i.==0) 0 $ ite (i.==2) 1000 $ ite (i.==3) 10000 $ 1


mapAt :: Expr (Int, Int) -> Expr [[Int]] -> Expr Int
mapAt pos chizu = nth (car pos) $ nth (cdr pos) chizu

vadd :: Expr (Int, Int) -> Expr (Int, Int) -> Expr (Int, Int)
vadd pos vect = cons (car pos + car vect) (cdr pos + cdr vect)

(dirValuePill:: Expr (Int, Int) -> Expr [[Int]] -> Expr (Int, Int) -> Expr Int, dirValuePillDef) =
  def3 "dirValuePill" $ \vect chizu manp ->
    let info = (mapAt manp chizu) in
    ite (info .== 0) 0 $
      (tileValue info) + (dirValuePill vect chizu $ vadd manp vect)`div`2

(dirValueTotal:: Expr (Int, Int) -> Expr World -> Expr Int, dirValueTotalDef) =
  def2 "dirValueTotal" $ \vect world ->
    let manP :: Expr Pos
        manP = car $ cdr $ car $ cdr world      
        chizu :: Expr [[Int]]
        chizu = car world
        
    in dirValuePill vect chizu manP


(step :: Expr AIState -> Expr World -> Expr (AIState,Int), stepDef) =
  def2 "step" $ \aist world ->
    let manPos :: Expr Pos
        manPos = car $ cdr $ car $ cdr world

        chizu :: Expr [[Int]]
        chizu = car world

        scoreN, scoreE, scoreS, scoreW :: Expr Int
        scoreN = dirValueTotal (cons 0 (-1)) world
        scoreE = dirValueTotal (cons 1    0) world
        scoreS = dirValueTotal (cons 0    1) world
        scoreW = dirValueTotal (cons (-1) 0) world

        d2 :: Expr Int
        d2 = ite ((scoreN .>= scoreE) + (scoreN .>= scoreS) + (scoreN .>= scoreW) .== 3) 0 $
             ite ((scoreE .>= scoreS) + (scoreE .>= scoreW) .== 2) 1 $
             ite (scoreS .>= scoreW) 2 $
             (3 :: Expr Int)

    in dbug (list [scoreN, scoreE, scoreS, scoreW])
        `Seq` cons aist d2

progn :: LMan ()
progn = do
  nthDef
  tileValueDef
  dirValuePillDef
  dirValueTotalDef
  stepDef

  expr $ cons (0 :: Expr AIState) $ Closure "step"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["debug"] -> do
      mapM_ putStrLn $ compile' progn
    _ -> do
      writeFile "../LambdaMan/aznyan.gcc" $ compile progn
