{-# LANGUAGE FlexibleInstances, GADTs, RecursiveDo, ScopedTypeVariables, RecordWildCards, RankNTypes, ImpredicativeTypes #-}

import qualified Data.Map as M
import Data.Maybe
import Data.Char
import Data.Function (on)
import Data.List (intercalate, sortBy)
import Debug.Trace
import Safe
import System.Environment
import System.IO
import System.IO.Unsafe
import System.FilePath
import System.Random
import System.Process
import Unsafe.Coerce
import Text.Printf
import Desugar
import DSL
import Lib
import System.Process


-----
type X = Int
type Direction = Int
type Clock = Int
type World0 = ([[Int]], (ManState, ([GhostState], FruitState)))
--               vit                    lives score
type ManState = (Int, (Pos, (Direction, (Int, Int  ))))
type GhostState = (Int, (Pos , Direction))
type FruitState = Int
type AIState = Int
type Pos = (Int, Int)


(step :: Expr AIState -> Expr World0 -> Expr (AIState, Int), stepDef) =
  def2 "step" $ \aist world0 -> 
    let
        gss :: Expr [GhostState]
        gss = car $ cdr $ cdr world0
        gs1 :: Expr GhostState
        gs1 = lhead  gss
        gY :: Expr Int
        gY = cdr $ cdr gs1
        
        d = ite (gY .== 1) 2 1
    in 
        cons aist d
      
progn :: LMan ()
progn = do
  libDef  
  stepDef  
  
  rtn $ cons (0 ::  Expr AIState) $ Closure "step"  
  
main :: IO ()
main = do
  writeFile "../LambdaMan/cornercase-think.gcc" $ compile progn    
