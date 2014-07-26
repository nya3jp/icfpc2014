{-# LANGUAGE FlexibleInstances, GADTs, RecursiveDo, ScopedTypeVariables, RecordWildCards, RankNTypes, ImpredicativeTypes #-}

import qualified Data.Map as M
import Data.Maybe
import Data.Char
import Debug.Trace
import System.Environment
import Unsafe.Coerce

import Desugar
import DSL07261800

-----
type X = Int
type World = ([[Int]], (ManState, (X, X)))
type ManState = (X, (Pos, X))
type AIState = X
type Pos = (Int, Int)


progn :: LMan ()
progn = do
  nth :: forall a. Expr ([a] -> Int -> a) <- nth'
  

  (tileValue :: Expr (Int -> Int)) <- fun1 $ \i ->                                              
    ite (i.==0) 0 $ ite (i.==2) 1000 $ ite (i.==3) 10000 $ 1

  let  mapAt :: Expr [[Int]] -> Expr Int -> Expr Int -> Expr Int
       mapAt chizu ix iy = (call2 nth (call2 nth chizu iy) ix)
       
       
  rec
     (dirValuePill :: Expr ([[Int]] -> Int -> Int -> Int))
       <- fun3 $ \chizu manX manY -> 
                 let info = (mapAt chizu manX manY) in
                 ite (info .== 0) 0 $ 
                 (call1 tileValue info) + (call3 dirValuePill chizu (manX+dx)  (manY+dy))`div`2



  dirValueTotal :: Expr ((Int, Int) -> World -> Int )
  dirValuetotal <- fun2 $ \vec world ->
    let chizu :: Expr [[Int]] 
        chizu = car world
        manPos :: Expr Pos 
        manPos = car $ cdr $ car $ cdr world
    in dirValuePill vec manPos chizu

  (step :: Expr (AIState -> World -> (AIState,Int))) <- fun2 $ \aist world ->
    let manPos :: Expr Pos 
        manPos = car $ cdr $ car $ cdr world
        manX :: Expr Int
        manX = car manPos
        manY :: Expr Int
        manY = cdr manPos
        
        chizu :: Expr [[Int]]
        chizu = car world


        
        scoreN, scoreE, scoreS, scoreW :: Expr Int
        scoreN = call2 dirValueTotal  (cons (0, -1)) world 
        scoreE = call2 dirValueTotal  (cons (1, 0) ) world 
        scoreS = call2 dirValueTotal  (cons (0, 1) ) world 
        scoreW = call2 dirValueTotal  (cons (-1, 0)) world 

        d2 :: Expr Int
        d2 = ite ((scoreN .>= scoreE) + (scoreN .>= scoreS) + (scoreN .>= scoreW) .== 3) 0 $
             ite ((scoreE .>= scoreS) + (scoreE .>= scoreW) .== 2) 1 $
             ite (scoreS .>= scoreW) 2 $
             (3 :: Expr Int)

    in
     
    dbugn (call3 dirValueN chizu manX manY) `Seq`
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
