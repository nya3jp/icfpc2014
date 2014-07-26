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


progn :: LMan ()
progn = do
  nth :: forall a. Expr ([a] -> Int -> a) <- nth'
  
  valOfItem :: Expr (Int -> Int)                     
    <- fun1 $ \i -> ite (i.==0) 0 $  ite (i.==2) 1000 $ ite (i.==3) 10000 $ 40
  
  (step :: Expr (AIState -> World -> (AIState,Int))) <- fun2 $ \aist world ->
    let manPos :: Expr Pos 
        manPos = car $ cdr $ car $ cdr world
        manX :: Expr Int
        manX = car manPos
        manY :: Expr Int
        manY = cdr manPos
        
        chizu :: Expr [[Int]]
        chizu = car world

        mapAt :: Expr Int -> Expr Int -> Expr Int
        mapAt ix iy = (call2 nth (call2 nth chizu iy) ix)
        
        calcScore :: (Expr Int, Expr Int) -> Expr Int
        calcScore (dx,dy) = call1 valOfItem (mapAt (manX+dx) (manY+dy))
        
        scoreN = calcScore (0,-1)
        scoreE = calcScore (1,0)
        scoreS = calcScore (0,1)
        scoreW = calcScore (-1,0)
        
        d2 :: Expr Int
        d2 = ite ((scoreN .>= scoreE) + (scoreN .>= scoreS) + (scoreN .>= scoreW) .== 3) 0 $
             ite ((scoreE .>= scoreS) + (scoreE .>= scoreW) .== 2) 1 $
             ite (scoreS .>= scoreW) 2 $
             (3 :: Expr Int)
    in
     
     
    dbugn scoreN `Seq` 
    dbugn scoreE `Seq` 
    dbugn scoreW `Seq` 
    dbugn scoreS `Seq` 
    cons aist d2
  expr $ cons (0 :: Expr AIState) step



main :: IO ()
main = do
  args <- getArgs
  case args of
    ["debug"] -> do
      putStrLn $ compile' progn
    _ -> do
      writeFile "../LambdaMan/joga-maya.gcc" $ compile progn
