{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import DSL

libDef :: LMan ()
libDef = do
  llengthDef
  ldropDef
  ltakeDef
  nthDef
  updDef
  getMatDef
  setMatDef
  lreverseDef

  dequeueDef

  mkArrayDef
  mkArrayGoDef
  peekDef
  peekGoDef
  pokeDef
  pokeGoDef

(&&&) :: Expr Int -> Expr Int -> Expr Int
a &&& b = a * b

(|||) :: Expr Int -> Expr Int -> Expr Int
a ||| b = lnot $ lnot a &&& lnot b

lnot :: Expr Int -> Expr Int
lnot e = 1 - e

cadr = car . cdr
caddr = car . cdr . cdr
cdddr = cdr . cdr . cdr

llength :: Expr [a] -> Expr Int
(llength, llengthDef) = def1 "llength" $ \xs ->
  ite (atom xs) 0 $ 1 + llength (ltail xs)

ltake :: Expr Int -> Expr [a] -> Expr [a]
(ltake, ltakeDef) = def2 "ltake" $ \i xs ->
  ite (i .== 0) lnull $ lcons (lhead xs) $ ldrop (i-1) (ltail xs)

ldrop :: Expr Int -> Expr [a] -> Expr [a]
(ldrop, ldropDef) = def2 "ldrop" $ \i xs ->
  ite (i .== 0) xs $ ldrop (i-1) (ltail xs)

(nth, nthDef) = def2 "nth" $ \i xs ->
  ite (i .== 0) (lhead xs) (nth (i-1) (ltail xs))

(upd, updDef) = def3 "upd" $ \i v xs ->
  ite (i .== 0) (lcons v $ ltail xs) (lcons (lhead xs) $ upd (i-1) v (ltail xs))

(getMat, getMatDef) = def3 "getMat" $ \x y m ->
  nth x $ nth y m

(setMat, setMatDef) = def4 "setMat" $ \x y v m ->
  upd y (upd x v $ nth y m) m

lreverse :: Expr [a] -> Expr [a]
lreverse = lreverseGo lnull

(lreverseGo, lreverseDef) = def2 "lreverseGo" $ \acc xs -> do
  ite (isNull xs) acc $ lreverseGo (lcons (lhead xs) acc) (ltail xs)

-- Queue

type Queue a = ([a], [a])

emptyQueue :: Expr (Queue a)
emptyQueue = cons lnull lnull

enqueue :: Expr a -> Expr (Queue a) -> Expr (Queue a)
enqueue v q =
  let hd = car q
      tl = cdr q
  in cons hd (lcons v tl)

dequeue :: Expr (Queue a) -> Expr (a, Queue a)
(dequeue, dequeueDef) = def1 "dequeue" $ \q ->
  let hd = car q
      tl = cdr q
  in ite (isNull hd)
       (dequeue $ cons (lreverse tl) lnull)
       (cons (lhead hd) $ cons (ltail hd) tl)

isEmptyQueue :: Expr (Queue a) -> Expr Int
isEmptyQueue q = (isNull $ car q) &&& (isNull $ cdr q)

-- Array

type Array a = (Int, Node a)
data Node a = Node

mkArray :: Expr [a] -> Expr (Array a)
(mkArray, mkArrayDef) = def1 "mkArray" $ \xs ->
  with (llength xs) $ \len ->
    cons len $ mkArrayGo 0 len xs

mkArrayGo :: Expr Int -> Expr Int -> Expr [a] -> Expr (Node a)
(mkArrayGo, mkArrayGoDef) = def3 "mkArrayGo" $ \l r xs ->
  let m = (l + r) `div` 2
  in ite (r - l .== 1)
     (cast $ lhead xs)
     (gcons (mkArrayGo l m xs) (mkArrayGo m r $ ldrop (m - l) xs))

peek :: Expr Int -> Expr (Array a) -> Expr a
(peek, peekDef) = def2 "peek" $ \ix arr -> peekGo ix 0 (car arr) (cdr arr)

peekGo :: Expr Int -> Expr Int -> Expr Int -> Expr (Node a) -> Expr a
(peekGo, peekGoDef) = def4 "peekGo" $ \ix l r node -> comp $ do
  let m = (l + r) `div` 2
  e $ite (r - l .== 1) (cast node) $
       ite (ix .< m)
         (peekGo ix l m (gcar node))
         (peekGo ix m r (gcdr node))

poke :: Expr Int -> Expr a -> Expr (Array a) -> Expr (Array a)
(poke, pokeDef) = def3 "poke" $ \ix v arr -> cons (car arr) (pokeGo ix 0 (car arr) v (cdr arr))

pokeGo :: Expr Int -> Expr Int -> Expr Int -> Expr a -> Expr (Node a) -> Expr (Node a)
(pokeGo, pokeGoDef) = def5 "pokeGo" $ \ix l r v node -> comp $ do
  let m = (l + r) `div` 2
  e $ ite (r - l .== 1) (cast v) $
      ite (ix .< m)
      (gcons (pokeGo ix l m v (gcar node)) (gcdr node))
      (gcons (gcar node) (pokeGo ix m r v (gcdr node)))
