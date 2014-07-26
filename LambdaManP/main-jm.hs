{-# LANGUAGE FlexibleInstances, GADTs, RecursiveDo, ScopedTypeVariables #-}

import qualified Data.Map as M
import Data.Maybe
import Data.Char
import Debug.Trace
import System.Environment

import Desugar
import DSL

-----
type X = Int
type World = (X, (ManState, (X, X)))
type ManState = (X, (Pos, X))
type AIState = X
type Pos = (Int, Int)


progn :: LMan ()
progn = do
  (step :: Expr (AIState -> World -> (AIState,Int))) <- fun2 $ \aist world ->
    let manPos :: Expr Pos 
        manPos = car $ cdr $ car $ cdr world
        manX :: Expr Int
        manX = car manPos
        manY :: Expr Int
        manY = cdr manPos
        d = ite (manY .<= 1) 3 $ ite (manX .< 17) 1 0 
    in
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
