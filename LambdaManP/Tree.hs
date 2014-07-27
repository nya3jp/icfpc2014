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

treeSize :: Int
treeSize = 20

data Tree = Tree 


tempty :: Expr Tree
tempty = unsafeCoerce $ Cons (Const 0) (Const 0)
