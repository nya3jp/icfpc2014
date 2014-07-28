{-# LANGUAGE FlexibleInstances, GADTs, RecursiveDo, ScopedTypeVariables, Rank2Types, LiberalTypeSynonyms, ImpredicativeTypes, NoMonoLocalBinds #-}

module Tree where

import Control.Monad.RWS hiding (local, Any)
import Control.Monad.State
import Control.Monad.Writer hiding (Any)
import Data.Maybe
import Debug.Trace
import Unsafe.Coerce

import Desugar
import DSL

defaultTreeSize :: Int
defaultTreeSize = 65536

data Tree = Tree 

tempty :: Expr Tree
tempty = unsafeCoerce $ (Const 0) 

tsingleton :: Expr Int -> Expr Tree
tsingleton x = unsafeCoerce $ x

tisSingleton :: Expr Tree -> Expr Int
tisSingleton = atom

tfromSingleton :: Expr Tree -> Expr Int
tfromSingleton = unsafeCoerce



tcons :: Expr Tree ->  Expr Tree ->  Expr Tree 
tcons =  unsafeCoerce $ Cons 


infix 5 ><
(><) :: Expr Tree ->  Expr Tree ->  Expr Tree 
(><) = tcons


tleft :: Expr Tree ->  Expr Tree
tleft=  unsafeCoerce $ Car

tright :: Expr Tree ->  Expr Tree
tright =  unsafeCoerce $ Cdr



--          key         value   
(tinsert :: Expr Int -> Expr Int -> Expr Tree -> Expr Tree, tinsertDef) =
  def3 "tinsert" $ \key value t -> tinsertN (Const defaultTreeSize) key value t
                                   
{- Given the tree  n, key, value and tree of size n, 
   return a tree whose key-th element is val-}
(tinsertN :: Expr Int -> Expr Int -> Expr Int -> Expr Tree -> Expr Tree, tinsertNDef) =
  def4 "tinsertN" $ \n key value t -> 
    let
      zeroCase =  ite (key .< (div n 2)) 
                  (tinsertN (div n 2) key value tempty >< tempty)
                  (tempty >< tinsertN (div n 2) (key - div n 2) value tempty)
      in 
      ite (n .<= 1) (tsingleton value) $
      ite (tisSingleton t) zeroCase $
      ite (key .< (div n 2)) (tinsertN (div n 2) key value (tleft t) >< tright t) $
      (tleft t >< tinsertN (div n 2) (key - (div n 2)) value (tright t))
    


--          key         
(tlookup :: Expr Int -> Expr Tree -> Expr Int, tlookupDef) =
  def2 "tlookup" $ \key t -> tlookupN (Const defaultTreeSize) key t


(tlookupN :: Expr Int -> Expr Int -> Expr Tree -> Expr Int, tlookupNDef) =
  def3 "tlookupN" $ \n key t -> 
    let
      zeroCase = Const 0
      in 
      ite (n .<= 1) (tfromSingleton t) $
      ite (tisSingleton t) zeroCase $
      ite (key .< (div n 2)) (tlookupN (div n 2) key (tleft t)) $
      (tlookupN (div n 2) (key - (div n 2)) (tright t))
    

