{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import DSL

libDef :: LMan ()
libDef = do
  llengthDef
  ldropDef
  ltakeDef
  nthDef
  nthOptDef
  updDef
  updOptDef
  getMatDef
  setMatDef
  lreverseDef
  -- lreplicateDef

  dequeueDef

  newArrayDef
  newArrayGoDef
  mkArrayDef
  mkArrayGoDef
  peekDef
  pokeDef
  pokeGoDef

  toMatDef
  toMatsDef
  newMatDef

infixr 3 &&&, |||

undef :: Expr a
undef = cast (c 0)


(&&&) :: Expr Int -> Expr Int -> Expr Int
a &&& b = ite a b 0

(|||) :: Expr Int -> Expr Int -> Expr Int
a ||| b = ite a 1 b

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
  ite (i .== 0) lnull $ lcons (lhead xs) $ ltake (i-1) (ltail xs)

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

chunkSize = 4 :: Expr Int

nthOpt :: Expr Int -> Expr [a] -> Expr a
(nthOpt, nthOptDef) = def2 "nthOpt" $ \i xs ->
  ite (i .== 0) (lhead xs) $
  ite (i .== 1) (lhead $ ltail xs) $
  ite (i .== 2) (lhead $ ltail $ ltail xs) $
  ite (i .== 3) (lhead $ ltail $ ltail $ ltail xs) $
  ite (i .== 4) (lhead $ ltail $ ltail $ ltail $ ltail xs) $
  ite (i .== 5) (lhead $ ltail $ ltail $ ltail $ ltail $ ltail xs) $
  ite (i .== 6) (lhead $ ltail $ ltail $ ltail $ ltail $ ltail $ ltail xs) $
  (lhead $ ltail $ ltail $ ltail $ ltail $ ltail $ ltail $ ltail xs)

updOpt :: Expr Int -> Expr a -> Expr [a] -> Expr [a]
(updOpt, updOptDef) = def3 "updOpt" $ \i v xs0 -> comp $
  with (ltail xs0) $ \xs1 -> e $ ite (i .== 0) (lcons v xs1) $ comp $
  with (ltail xs1) $ \xs2 -> e $ ite (i .== 1) (lcons (lhead xs0) $ lcons v xs2) $ comp $
  with (ltail xs2) $ \xs3 -> e $ ite (i .== 2) (lcons (lhead xs0) $ lcons (lhead xs1) $ lcons v xs3) $ comp $
  with (ltail xs3) $ \xs4 -> e $ lcons (lhead xs0) $ lcons (lhead xs1) $ lcons (lhead xs2) $ lcons v xs4

{-
newArray :: Expr Int -> Expr a -> Expr (Array a)
(newArray, newArrayDef) = def2 "newArray" $ \n v -> cons n $ newArrayGo n v

lreplicate :: Expr Int -> Expr a -> Expr [a]
(lreplicate, lreplicateDef) = def2 "lreplicate" $ \n v ->
  ite (n .== 0) lnull $ lcons v $ lreplicate (n-1) v

newArrayGo :: Expr Int -> Expr a -> Expr (Node a)
(newArrayGo, newArrayGoDef) = def2 "newArrayGo" $ \n v ->
  ite (n .<= chunkSize) (cast $ lreplicate n v) $
    let m = n `div` 2
    in gcons (newArrayGo m v) (newArrayGo (n-m) v)

mkArray :: Expr [a] -> Expr (Array a)
(mkArray, mkArrayDef) = def1 "mkArray" $ \xs -> comp $
  with (llength xs) $ \len ->
    e $ cons len $ mkArrayGo len xs

mkArrayGo :: Expr Int -> Expr [a] -> Expr (Node a)
(mkArrayGo, mkArrayGoDef) = def2 "mkArrayGo" $ \n xs ->
  ite (n .<= chunkSize) (cast $ ltake n xs) $
    let m = n `div` 2
    in gcons (mkArrayGo m xs) (mkArrayGo (n-m) $ ldrop m xs)

peek :: Expr Int -> Expr (Array a) -> Expr a
(peek, peekDef) = def2 "peek" $ \ix arr -> comp $ do
  with3 (car arr) (cdr arr) undef $ \n node m -> do
    while (n .> chunkSize) $ do
      m ~= n `div` 2
      cond (ix .< m)
        (n ~= m >> node ~= gcar node)
        (n ~= n - m >> ix ~= ix - m >> node ~= gcdr node)
    e $ nthOpt ix (cast node)

poke :: Expr Int -> Expr a -> Expr (Array a) -> Expr (Array a)
(poke, pokeDef) = def3 "poke" $ \ix v arr -> cons (car arr) (pokeGo ix (car arr) v (cdr arr))

pokeGo :: Expr Int -> Expr Int -> Expr a -> Expr (Node a) -> Expr (Node a)
(pokeGo, pokeGoDef) = def4 "pokeGo" $ \ix n v node -> do
  ite (n .<= chunkSize) (cast $ upd ix v $ cast node) $ comp $ do
    let m = n `div` 2
    e $ ite (ix .< m)
      (gcons (pokeGo ix m v (gcar node)) (gcdr node))
      (gcons (gcar node) (pokeGo (ix-m) (n-m) v (gcdr node)))
-}

newArray :: Expr Int -> Expr a -> Expr (Array a)
(newArray, newArrayDef) = def2 "newArray" $ \n v -> cons n $ newArrayGo 0 n v

newArrayGo :: Expr Int -> Expr Int -> Expr a -> Expr (Node a)
(newArrayGo, newArrayGoDef) = def3 "newArrayGo" $ \l r v ->
  let m = (l + r) `div` 2
  in ite (r - l .== 1)
     (cast v)
     (gcons (newArrayGo l m v) (newArrayGo m r v))

mkArray :: Expr [a] -> Expr (Array a)
(mkArray, mkArrayDef) = def1 "mkArray" $ \xs -> comp $
  with (llength xs) $ \len ->
    e $ cons len $ mkArrayGo 0 len xs

mkArrayGo :: Expr Int -> Expr Int -> Expr [a] -> Expr (Node a)
(mkArrayGo, mkArrayGoDef) = def3 "mkArrayGo" $ \l r xs ->
  let m = (l + r) `div` 2
  in ite (r - l .== 1)
     (cast $ lhead xs)
     (gcons (mkArrayGo l m xs) (mkArrayGo m r $ ldrop (m - l) xs))

{-
peek :: Expr Int -> Expr (Array a) -> Expr a
(peek, peekDef) = def2 "peek" $ \ix arr -> peekGo ix 0 (car arr) (cdr arr)

peekGo :: Expr Int -> Expr Int -> Expr Int -> Expr (Node a) -> Expr a
(peekGo, peekGoDef) = def4 "peekGo" $ \ix l r node -> comp $ do
let m = (l + r) `div` 2
e $ite (r - l .== 1) (cast node) $
ite (ix .< m)
(peekGo ix l m (gcar node))
(peekGo ix m r (gcdr node))
-}

peek :: Expr Int -> Expr (Array a) -> Expr a
(peek, peekDef) = def2 "peek" $ \ix arr -> comp $ do
  with4 0 (car arr) (cdr arr) undef $ \l r node m -> do
    while (r - l ./= 1) $ do
      m ~= (l + r) `div` 2
      cond (ix .< m)
        (r ~= m >> node ~= gcar node)
        (l ~= m >> node ~= gcdr node)
    e $ node

poke :: Expr Int -> Expr a -> Expr (Array a) -> Expr (Array a)
(poke, pokeDef) = def3 "poke" $ \ix v arr -> cons (car arr) (pokeGo ix 0 (car arr) v (cdr arr))

pokeGo :: Expr Int -> Expr Int -> Expr Int -> Expr a -> Expr (Node a) -> Expr (Node a)
(pokeGo, pokeGoDef) = def5 "pokeGo" $ \ix l r v node -> comp $ do
  let m = (l + r) `div` 2
  e $ ite (r - l .== 1) (cast v) $
      ite (ix .< m)
      (gcons (pokeGo ix l m v (gcar node)) (gcdr node))
      (gcons (gcar node) (pokeGo ix m r v (gcdr node)))

type Mat a = Array (Array a)

peekMat :: Expr Int -> Expr Int -> Expr (Mat a) -> Expr a
peekMat x y m = peek x $ peek y m

pokeMat :: Expr Int -> Expr Int -> Expr a -> Expr (Mat a) -> Expr (Mat a)
pokeMat x y v m = poke y (poke x v $ peek y m) m

newMat :: Expr Int -> Expr Int -> Expr a -> Expr (Mat a)
(newMat, newMatDef) = def3 "newMat" $ \x y v -> comp $ do
  with (newArray x v) $ \row -> e $ newArray y row

toMat :: Expr [[a]] -> Expr (Mat a)
(toMat, toMatDef) = def1 "toMat" $ \mm -> mkArray $ toMats mm

toMats :: Expr [[a]] -> Expr [Array a]
(toMats, toMatsDef) = def1 "toMats" $ \rows ->
  ite (isNull rows) lnull $
  lcons (mkArray $ lhead rows) (toMats $ ltail rows)
