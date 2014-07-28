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
import Vect
import System.Process

---- vector libraries




-----
data X = X
exprX :: Expr X
exprX = cast (Const 0)
type Clock = Int
type Environ = (AIState,World)
type World = ([[Int]], (ManState, ([GhostState], FruitState)))

--               vit                    lives score
type ManState = (Int, (Pos, (Direction, (Int, Int  ))))
type GhostState = (Int, (Pos , Direction))
type FruitState = Int
type AIState = ((Mat Int),(Clock,X))

manStateE :: Expr Environ -> Expr ManState
manStateE = caddr

chizuE :: Expr Environ -> Expr (Mat Int)
chizuE = car . car

manPosE :: Expr Environ -> Expr Pos
manPosE = cadr . manStateE


(dirValueTotal :: Expr Environ -> Expr Pos -> Expr Int, dirValueTotalDef) = 
  def2 "dirValueTotal" $ \env vec -> comp $ do
    e $ vpeek (manPosE env `vadd` vec) (chizuE env)

(step :: Expr AIState -> Expr World -> Expr (AIState, Int), stepDef) =
  def2 "step" $ \aist world -> comp $ do
    let 
        env :: Expr Environ
        env = cons aist world
        
        clk :: Expr Clock
        clk = cadr aist
        
        chizu :: Expr (Mat Int)
        chizu = car aist
              
        manState :: Expr ManState
        manState = car $ cdr world        
        manP :: Expr Pos
        manP = car $ cdr $ manState

      
        newAist :: Expr AIState
        newAist = cons (vpoke manP (negate clk) chizu) $
                  cons (clk+1) (cdr $cdr aist)
        
        scoreN, scoreE, scoreS, scoreW :: Expr Int
        scoreN = dirValueTotal env vecN 
        scoreE = dirValueTotal env vecE 
        scoreS = dirValueTotal env vecS 
        scoreW = dirValueTotal env vecW 

        dirToGo :: Expr Direction
        dirToGo 
           = ite ((scoreN .>= scoreE) + (scoreN .>= scoreS) + (scoreN .>= scoreW) .== 3) 0 $
             ite ((scoreE .>= scoreS) + (scoreE .>= scoreW) .== 2) 1 $
             ite (scoreS .>= scoreW) 2 $
             (3 :: Expr Int)
        
    e $ cons newAist dirToGo
      
progn :: LMan ()
progn = do
  libDef  
  stepDef  
  dirValueTotalDef
  


  let chizu = toMat $ car (Var (-1) 0 :: Expr World)
      aist :: Expr AIState
      aist = cons chizu (cons (Const 0) exprX) 
  rtn $ cons aist $ Closure "step"

main :: IO ()
main = do
  writeFile "../LambdaMan/mito-run.gcc" $ compile progn    
