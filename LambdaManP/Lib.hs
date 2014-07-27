{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import DSL

libDef :: LMan ()
libDef = do
  nthDef
  updDef
  getMatDef
  setMatDef
  lreverseDef

  dequeueDef

(&&&) :: Expr Int -> Expr Int -> Expr Int
a &&& b = a * b

(|||) :: Expr Int -> Expr Int -> Expr Int
a ||| b = lnot $ lnot a &&& lnot b

lnot :: Expr Int -> Expr Int
lnot e = 1 - e

cadr = car . cdr
caddr = car . cdr . cdr
cdddr = cdr . cdr . cdr

(nth, nthDef) = def2 "nth" $ \i xs ->
  ite (i .== 0) (lhead xs) (nth (i-1) (ltail xs))

(upd, updDef) = def3 "upd" $ \i v xs ->
  ite (i .== 0) (lcons v $ ltail xs) (lcons (lhead xs) $ upd (i-1) v (ltail xs))

(getMat, getMatDef) = def3 "getMat" $ \x y m ->
  nth x $ nth y m

(setMat, setMatDef) = def4 "setMat" $ \x y v m ->
  upd y (upd x v $ nth y m) m

lreverse :: Expr [a] -> Expr [a]
lreverse = lreverse' lnull

(lreverse', lreverseDef) = def2 "lreverse'" $ \acc xs -> do
  ite (isNull xs) acc $ lreverse' (lcons (lhead xs) acc) (ltail xs)

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
