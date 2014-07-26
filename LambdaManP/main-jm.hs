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
  
--   rec
--     (esaN :: Expr (Int -> Int)) <- fun1 $ \iy -> 
--          esaN2 iy (mapAt manX iy)
--     (esaN2 :: Expr (Int -> Int -> Int)) <- fun2 $ \iy atInfo ->
--          ite (atInfo .== 2) 1 $ ite (atInfo .==0) 99 $ esaN (iy-1)


  let  mapAt :: Expr [[Int]] -> Expr Int -> Expr Int -> Expr Int
       mapAt chizu ix iy = (call2 nth (call2 nth chizu iy) ix)
  rec
     (searchN :: Expr ([[Int]] -> Int -> Int -> Int))
       <- fun3 $ \chizu manX manY -> 
                 call4 searchN2 (mapAt chizu manX manY) chizu manX manY
     (searchN2 :: Expr (Int -> [[Int]] -> Int -> Int -> Int))
       <- fun4 $ \info chizu manX manY -> 
         ite (info .== 2) 1 $ ite (info .== 0) 99 $ 44
      --     call3 searchN chizu manX (manY-1)





  
  (step :: Expr (AIState -> World -> (AIState,Int))) <- fun2 $ \aist world ->
    let manPos :: Expr Pos 
        manPos = car $ cdr $ car $ cdr world
        manX :: Expr Int
        manX = car manPos
        manY :: Expr Int
        manY = cdr manPos
        d = ite (manY .<= 1) 3 $ ite (manX .< 17) 1 0 
        
        chizu :: Expr [[Int]]
        chizu = car world


        
        scoreN :: Expr Int
        scoreN = call3 searchN chizu manX manY
    in
     
     
    dbugn scoreN `Seq` 
    cons aist d
  expr $ cons (0 :: Expr AIState) step



main :: IO ()
main = do
  args <- getArgs
  case args of
    ["debug"] -> do
      putStrLn $ compile' progn
    _ -> do
      writeFile "../LambdaMan/joga-maya.gcc" $ compile progn
