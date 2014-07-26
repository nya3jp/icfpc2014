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
type Direction = Int
type World = ([[Int]], (ManState, ([GhostState], FruitState)))

--               vit                    lives score
type ManState = (Int, (Pos, (Direction, (Int, Int  ))))
type GhostState = (Int, (Pos , Direction))
type FruitState = Int
type AIState = X
type Pos = (Int, Int)

(tileValue :: Expr Int -> Expr Int, tileValueDef) = def1 "tileView" $ \i ->
  ite (i.==0) 0 $ ite (i.==2) 100 $ ite (i.==3) 1000 $ 1


int_min :: Num a => a
int_min = -2^(31)

mapAt :: Expr Pos -> Expr [[Int]] -> Expr Int
mapAt pos chizu = nth (car pos) $ nth (cdr pos) chizu

vadd :: Expr Pos -> Expr Pos -> Expr Pos
vadd pos vect = cons (car pos + car vect) (cdr pos + cdr vect)

vsub :: Expr Pos -> Expr Pos -> Expr Pos
vsub pos vect = cons (car pos - car vect) (cdr pos - cdr vect)

vinner :: Expr Pos -> Expr Pos -> Expr Int
vinner pos vect = (car pos * car vect) + (cdr pos * cdr vect)

(dirValuePill:: Expr Pos -> Expr [[Int]] -> Expr Pos -> Expr Int, dirValuePillDef) =
  def3 "dirValuePill" $ \vect chizu manp ->
    let info = (mapAt manp chizu) in
    ite (info .== 0) 0 $
      (tileValue info) + (dirValuePill vect chizu $ vadd manp vect)`div`2

(dirValueGhost1 :: Expr Pos -> Expr GhostState -> Expr Int -> Expr Pos -> Expr Int, dirValueGhost1Def) = 
  def4 "dirValueGhost1" $ \vect gs1 ppflag manp ->
    let ghostp :: Expr Pos 
        ghostp = car $ cdr gs1 
        
        vecDiff = vsub ghostp manp
        dist2 :: Expr Int
        dist2 = vinner vecDiff vecDiff
        
    in ite ppflag ((100 * vinner vect vecDiff))
                  ((negate 100 * vinner vect vecDiff) `div` dist2)


(dirValueGhosts :: Expr Pos -> Expr [GhostState] -> Expr Int -> Expr Pos -> Expr Int, dirValueGhostsDef) = 
  def4 "dirValueGhosts" $ \vect gss ppflag manp ->
    ite (atom gss) 0 $ dirValueGhost1 vect (lhead gss) ppflag manp + 
                       dirValueGhosts vect (ltail gss) ppflag manp  



(dirValueTotal:: Expr Pos -> Expr World -> Expr Int, dirValueTotalDef) =
  def2 "dirValueTotal" $ \vect world -> 
    let manP :: Expr Pos
        manP = car $ cdr $ manState

        manState :: Expr ManState
        manState = car $ cdr world

        powerPillFlag :: Expr Int
        powerPillFlag = car manState
   
        chizu :: Expr [[Int]]
        chizu = car world
        
        gss :: Expr [GhostState]
        gss = car $ cdr $ cdr world
        
        nextP :: Expr Pos
        nextP = vadd manP vect
    in 
        ite (mapAt nextP chizu .== 0) int_min $
        dirValuePill vect chizu manP + 
          dirValueGhosts vect gss powerPillFlag manP


(step :: Expr AIState -> Expr World -> Expr (AIState,Int), stepDef) =
  def2 "step" $ \aist world ->
    let scoreN, scoreE, scoreS, scoreW :: Expr Int
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
  dirValueGhost1Def  
  dirValueGhostsDef  
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
