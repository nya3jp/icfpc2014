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

---- vector libraries




-----
type X = Int
type Direction = Int
type Clock = Int
type AllState = (AiState,World)
type World = (Mat Int, (ManState, ([GhostState], FruitState)))
type World0 = ([[Int]], (ManState, ([GhostState], FruitState)))
--               vit                    lives score
type ManState = (Int, (Pos, (Direction, (Int, Int  ))))
type GhostState = (Int, (Pos , Direction))
type FruitState = Int
type AIState = ((Mat Int),(Clock,X))
type Pos = (Int, Int)


(step :: Expr AIState -> Expr World -> Expr (AIState, Int), stepDef) =
  def2 "step" $ \aist world -> 
    let 
        allState = cons 
        world = cons chizu (cdr world0)
        clk :: Expr Clock
        clk = car $ cdr aist
      
        manState :: Expr ManState
        manState = car $ cdr world        
        manP :: Expr Pos
        manP = car $ cdr $ manState


      
        newAist :: Expr AIState
        newAist = cons (mapPoke manP (negate clk) chizu) $
                  cons (clk+1) (cdr $cdr aist)
        dirToGo = 0
    in 
        cons newAist dirToGo
      
progn :: LMan ()
progn = do
  libDef  
  stepDef  
  
  let chizu = toMat $ car (Var (-1) 0 :: Expr World0)
  rtn $ cons (cons chizu (cons 0 0) :: Expr AIState) $ Closure "step"

main :: IO ()
main = do
  writeFile "../LambdaMan/cornercase-think.gcc" $ compile progn    
