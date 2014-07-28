module Vect where


import DSL
import Lib

type Pos = (Int,Int)
type V4 = ((Int,Int),(Int,Int))

vecN,vecE,vecS,vecW :: Expr Pos
vecN = (cons 0 (-1))
vecE = (cons 1    0)
vecS = (cons 0    1)
vecW = (cons (-1) 0)
      
vzero4 :: Expr V4
vzero4 = cons (cons 0 0) (cons 0 0)

type Direction = Int
       
vofd :: Expr Direction -> Expr Pos
vofd d = ite (d.<=1) (ite (d.==0) vecN vecE) $
         ite (d.==2) vecS vecW


vpeek :: Expr Pos -> Expr (Mat Int) -> Expr Int
vpeek pos chizu = peekMat (car pos) (cdr pos) chizu

vpoke :: Expr Pos -> Expr Int -> Expr (Mat Int) -> Expr (Mat Int)
vpoke pos val chizu = pokeMat (car pos) (cdr pos) val chizu

veq :: Expr Pos -> Expr Pos -> Expr Int
veq  pos vect = (car pos .== car vect) * (cdr pos .== cdr vect)

vadd :: Expr Pos -> Expr Pos -> Expr Pos
vadd pos vect = cons (car pos + car vect) (cdr pos + cdr vect)
vadd4 :: Expr V4 -> Expr V4 -> Expr V4
vadd4 pos vect = cons (car pos `vadd` car vect) (cdr pos `vadd` cdr vect)

maxIndex4 :: Expr V4 -> Expr Int
maxIndex4 q = comp $
  with4 (car $ car q) (cdr$ car q) (car$cdr q) (cdr$cdr q) $ \c0 c1 c2 c3 -> 
    cond (c0 .> c1 &&& c0 .> c2 &&& c0 .> c3) (e $ c 0) $
    cond (c1 .> c2 &&& c1 .> c3 &&& c1 .> c0) (e $ c 1) $
    cond (c2 .> c3 &&& c2 .> c0 &&& c2 .> c1) (e $ c 2) $
    cond (c3 .> c0 &&& c3 .> c1 &&& c3 .> c2) (e $ c 3) $
    e $ c (-1)


vscale :: Expr Int -> Expr Pos -> Expr Pos
vscale s v = cons (s * car v) (s * cdr v)

vscale4 :: Expr Int -> Expr V4 -> Expr V4
vscale4 s v = cons (s `vscale` car v) (s `vscale` cdr v)

vsub :: Expr Pos -> Expr Pos -> Expr Pos
vsub pos vect = cons (car pos - car vect) (cdr pos - cdr vect)

vinner :: Expr Pos -> Expr Pos -> Expr Int
vinner pos vect = (car pos * car vect) + (cdr pos * cdr vect)

vnorm :: Expr Pos -> Expr Int
vnorm v = vinner v v

vrotR :: Expr Pos -> Expr Pos
vrotR vect = cons (negate $ cdr vect) (car vect)

vrotL :: Expr Pos -> Expr Pos
vrotL vect = cons (cdr vect) (negate $ car vect)
