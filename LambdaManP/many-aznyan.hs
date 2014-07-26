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


pillParam ::  Int
pillParam = 100
powerPillParam ::  Int
powerPillParam = 2000
ghostPillParam ::  Int
ghostPillParam = negate 5000
ghostPillParamF ::  Int
ghostPillParamF = 10000


ghostAuraParamF :: Int
ghostAuraParamF = 1600

ghostAuraParam :: Int
ghostAuraParam = negate 800


(tileValue :: Expr Int -> Expr Int, tileValueDef) = def1 "tileView" $ \i ->
  ite (i.==0) 0 $ 
  ite (i.==2) (Const pillParam) $ 
  ite (i.==3) (Const powerPillParam) $ 
  1


int_min :: Num a => a
int_min = -2^(31)

mapAt :: Expr Pos -> Expr [[Int]] -> Expr Int
mapAt pos chizu = nth (car pos) $ nth (cdr pos) chizu

veq :: Expr Pos -> Expr Pos -> Expr Int
veq  pos vect = (car pos .== car vect) * (cdr pos .== cdr vect)


vadd :: Expr Pos -> Expr Pos -> Expr Pos
vadd pos vect = cons (car pos + car vect) (cdr pos + cdr vect)

vsub :: Expr Pos -> Expr Pos -> Expr Pos
vsub pos vect = cons (car pos - car vect) (cdr pos - cdr vect)

vinner :: Expr Pos -> Expr Pos -> Expr Int
vinner pos vect = (car pos * car vect) + (cdr pos * cdr vect)

vrotR :: Expr Pos -> Expr Pos
vrotR vect = cons (negate $ cdr vect) (car vect)

vrotL :: Expr Pos -> Expr Pos
vrotL vect = cons (cdr vect) (negate $ car vect)

(dirValuePill:: Expr Int -> Expr Pos -> Expr World -> Expr Pos -> Expr Int, dirValuePillDef) =
  def4 "dirValuePill" $ \depth vect world manp ->
    let info = (mapAt manp chizu) 
        chizu :: Expr [[Int]]
        chizu = car world
        gss :: Expr [GhostState]
        gss = car $ cdr $ cdr world
        
        manState :: Expr ManState
        manState = car $ cdr world
        
        powerPillFlag :: Expr Int
        powerPillFlag = car manState

        subScore = (dirValuePill (depth+1) (vrotR vect) world (vsub manp vect)
                 + dirValuePill (depth+1) (vrotL vect) world (vsub manp vect)) `div` 2
    in 
    ite (depth .>= 3) 0 $
    ite (info .== 0) subScore $
    ite (isGhostThere gss manp) (ite powerPillFlag 10000 (negate 5000))
    (tileValue info) + (dirValuePill depth vect world $ vadd manp vect)*9`div`10

(dirValueGhost1 :: Expr Pos -> Expr GhostState -> Expr Int -> Expr Pos -> Expr Int, dirValueGhost1Def) = 
  def4 "dirValueGhost1" $ \vect gs1 ppflag manp ->
    let ghostp :: Expr Pos 
        ghostp = car $ cdr gs1 
        
        vecDiff = vsub ghostp manp
        dist2 :: Expr Int
        dist2 = vinner vecDiff vecDiff
        
    in ite ppflag ((Const ghostAuraParamF * vinner vect vecDiff) `div` dist2)
                  ((Const ghostAuraParam * vinner vect vecDiff) `div` dist2)


(dirValueGhosts :: Expr Pos -> Expr [GhostState] -> Expr Int -> Expr Pos -> Expr Int, dirValueGhostsDef) = 
  def4 "dirValueGhosts" $ \vect gss ppflag manp ->
    ite (atom gss) 0 $ dirValueGhost1 vect (lhead gss) ppflag manp + 
                       dirValueGhosts vect (ltail gss) ppflag manp  


(isGhostThere :: Expr [GhostState] -> Expr Pos -> Expr Int, isGhostThereDef) = 
  def2 "isGhostThere" $ \gss pos -> 
  let gs1 = lhead gss
      ghostPos = car $ cdr gs1
      ghostVit = car gs1      
      
  in ite (atom gss) 0 $ 
     ite ((ghostPos `veq` pos) - (ghostVit .== 2) .== 1) 1 $ 
     isGhostThere (ltail gss) pos



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
        dirValuePill 0 vect world manP 
          + dirValueGhosts vect gss powerPillFlag manP


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
  isGhostThereDef
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
