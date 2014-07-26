{-# LANGUAGE FlexibleInstances, GADTs, RecursiveDo, ScopedTypeVariables, RecordWildCards, RankNTypes, ImpredicativeTypes, NoMonoLocalBinds #-}

import qualified Data.Map as M
import Data.Maybe
import Data.Char
import Debug.Trace
import System.Environment
import Unsafe.Coerce
import Control.Applicative

import Desugar
import DSL

import Prelude hiding (even, odd)

-----

data State = Int

cadr = car . cdr
caddr = car . cdr . cdr
cdddr = cdr . cdr . cdr

type Map = [[Int]]
type LMState = Int
type Ghost = (Int, (Int, Int), Int) -- (vial, loc, direction)

-- vital: 0: standard:, 1: fright mode, 2: invisible
-- direction: 0: up, 1: right, 2: down, 3: left

type WorldState = (Map, (Int, (Int, Int)))

debug = e. dbug

{-
progn :: LMan ()
progn = do
  rec
    (nth :: forall a. Expr (Int -> [a] -> a)) <- unsafeCoerce $ fun2 $ \xs i ->
      ite (i .== 0)
        (lhead xs)
        (call2 nth (i-1) (ltail xs))

    (upd :: forall a. Expr (Int -> a -> [a] -> [a])) <- unsafeCoerce $ fun3 $ \i v xs ->
      ite (i .== 0)
        (lcons v (ltail xs))
        (lcons (lhead xs) (call3 upd (i-1) v $ ltail xs))

    (llength :: forall a. Expr ([a] -> Int)) <- unsafeCoerce $ fun1 $ \xs ->
      ite (atom xs) 0 (1 + call1 llength (ltail xs))

    (getMap :: Expr (Int -> Int -> Map -> Int)) <- fun3 $ \x y m ->
      call2 nth x $ call2 nth y m

    (setMap :: Expr (Int -> Int -> Int -> Map -> Map)) <- fun4 $ \x y v m ->
      call3 upd y (call3 upd x v $ call2 nth y m) m

  step <- fun2 $ \(_state :: Expr State) (world :: Expr WorldState) -> comp $ do
    let mm = car world :: Expr [[Int]]
        lmstat = cadr world
        gstats = caddr world
        floc = cdddr world

    debug $ call3 getMap 0 0 mm

    debug $ call1 llength mm
    debug $ call1 (unsafeCoerce llength) (call2 nth 0 mm)
    debug $ car world
    debug $ cadr world
    debug $ caddr world
    debug $ cdddr world

  expr $ cons (0 :: Expr Int) step
  return ()
-}


(even :: Expr Int -> Expr Int, evenDef) = def1 "even" $ \i ->
  ite (i .== 0) 1 (odd $ i-1)

(odd :: Expr Int -> Expr Int, oddDef) = def1 "odd" $ \i ->
  ite (i .== 0) 0 (even $ i-1)

progn :: LMan ()
progn = do
  evenDef
  oddDef

  expr $ comp $ do
    debug $ even 5
    debug $ odd 5

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["debug"] -> do
      mapM_ putStrLn $ compile' progn
    _ -> do
      putStrLn $ compile progn
