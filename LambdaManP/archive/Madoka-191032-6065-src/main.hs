{-# LANGUAGE FlexibleInstances, GADTs, RecursiveDo, ScopedTypeVariables, RecordWildCards, RankNTypes, ImpredicativeTypes, NoMonoLocalBinds #-}

import Data.Maybe
import System.Environment
import Control.Applicative hiding (Const)

import DSL
import Lib

import Prelude hiding (even, odd)

-----

type Map = Mat Int

data X = X
type Pos = (Int, Int)

type World = ([[Int]], (ManState, ([GhostState], FruitState)))
type ManState = (X, (Pos, X))
type GhostState = (Int, (Pos, Int))
type FruitState = Int

type AIState = Mat Int

getWidth :: Expr Map -> Expr Int
getWidth e = car $ peek 0 e

getHeight :: Expr Map -> Expr Int
getHeight e = car e

peekMap :: Expr Pos -> Expr Map -> Expr Int
peekMap pos bd = peekMat (car pos) (cdr pos) bd

pokeMap :: Expr Pos -> Expr Int -> Expr Map -> Expr Map
pokeMap pos v bd = pokeMat (car pos) (cdr pos) v bd

-- vital: 0: standard:, 1: fright mode, 2: invisible
-- direction: 0: up, 1: right, 2: down, 3: left

vadd :: Expr Pos -> Expr Pos -> Expr Pos
vadd a b = cons (car a + car b) (cdr a + cdr b)

toQueue :: Expr [a] -> Expr (Queue (Int, a))
(toQueue, toQueueDef) = def1 "toQueue" $ \xs -> comp $
  with emptyQueue $ \q -> do
    while (lnot $ isNull xs) $ do
      q ~= enqueue (cons (c 0) $ lhead xs) q
      xs ~= ltail xs
    e q

withVects :: ([Expr Pos] -> CExpr r ()) -> CExpr r ()
withVects f =
  --     N             E         S          W
  with4 (cons 0 (-1)) (cons 1 0) (cons 0 1) (cons (-1) 0) $
    \v1 v2 v3 v4 -> f [v1, v2, v3, v4]

bfs :: Expr Map -> Expr Pos -> Expr Int -> Expr Int -> Expr Int
(bfs, bfsDef) = def4 "bfs" $ \bd start target depLimit -> ite (peekMap start bd .== 0) inf $ comp $
  with3 inf (toQueue $ list [start]) undef $ \out q qq ->
  with undef $ \cell ->
  withVects $ \[v1, v2, v3, v4] -> do
    while (lnot $ isEmptyQueue q) $ do
      qq ~= dequeue q
      q ~= cdr qq

      let dep = car $ car qq
          pos = cdr $ car qq

      cell ~= peekMap pos bd

      -- trace (c 10002, dep, depLimit, pos, cell)
      -- traceMap 123 bd

      cond (cell .== target)
        ( do
             out ~= dep
             q ~= emptyQueue
        )
        ( lwhen (dep + 1 .<= depLimit &&& cell ./= 0) $ do
             bd ~= pokeMat (car pos) (cdr pos) 0 bd
             q ~= enqueue (cons (dep + 1) $ vadd pos v1) q
             q ~= enqueue (cons (dep + 1) $ vadd pos v2) q
             q ~= enqueue (cons (dep + 1) $ vadd pos v3) q
             q ~= enqueue (cons (dep + 1) $ vadd pos v4) q
        )

    e out

inf :: Expr Int
inf = 99999

quota :: Expr Int
quota = 30

tickLimit = 999999

veq :: Expr Pos -> Expr Pos -> Expr Int
veq p q = car p .== car q &&& cdr p .== cdr q

updPoss :: Expr [Pos] -> Expr Int -> Expr Map -> Expr Map
(updPoss, updPossDef) = def3 "updPoss" $ \ps v bd ->
  ite (isNull ps) bd $ updPoss (ltail ps) v $ pokeMap (lhead ps) v bd

paint :: Expr Map -> Expr [Pos] -> Expr Map
(paint, paintDef) = def2 "paint" $ \bd starts -> comp $ do

  bd ~= updPoss starts 0 bd
  with3 (newMat (getWidth bd) (getHeight bd) inf) (toQueue starts) undef $ \ret q qq ->
    with4 0 undef undef undef $ \tick cell dep pos ->
    withVects $ \[v1, v2, v3, v4] -> do
    while (tick .< tickLimit &&& lnot (isEmptyQueue q)) $ do
      tick ~= tick + 1

      qq ~= dequeue q
      q ~= cdr qq

      dep ~= car (car qq)
      pos ~= cdr (car qq)

      cell ~= peekMat (car pos) (cdr pos) bd

      ret ~= pokeMap pos dep ret

      lwhen (peekMap (vadd pos v1) bd ./= 0) $ do
        bd ~= pokeMap (vadd pos v1) 0 bd
        q ~= enqueue (cons (dep + 1) $ vadd pos v1) q

      lwhen (peekMap (vadd pos v2) bd ./= 0) $ do
        bd ~= pokeMap (vadd pos v2) 0 bd
        q ~= enqueue (cons (dep + 1) $ vadd pos v2) q

      lwhen (peekMap (vadd pos v3) bd ./= 0) $ do
        bd ~= pokeMap (vadd pos v3) 0 bd
        q ~= enqueue (cons (dep + 1) $ vadd pos v3) q

      lwhen (peekMap (vadd pos v4) bd ./= 0) $ do
        bd ~= pokeMap (vadd pos v4) 0 bd
        q ~= enqueue (cons (dep + 1) $ vadd pos v4) q

    trace (c 77777, tick)
    e ret

{-
paint :: Expr Map -> Expr [Pos] -> Expr Map
(paint, paintDef) = def2 "paint" $ \bd starts -> comp $ do
  bd ~= updPoss starts 1 bd
  with3 (newMat (getWidth bd) (getHeight bd) inf) (toQueue starts) undef $ \ret q qq ->
    with4 0 undef undef undef $ \tick cell dep pos ->
    withVects $ \[v1, v2, v3, v4] -> do
    while (tick .< tickLimit &&& lnot (isEmptyQueue q)) $ do
      tick ~= tick + 1

      qq ~= dequeue q
      q ~= cdr qq

      dep ~= car (car qq)
      pos ~= cdr (car qq)

      cell ~= peekMat (car pos) (cdr pos) bd

      lwhen (cell ./= 0) $ do
        ret ~= pokeMap pos dep ret
        bd  ~= pokeMap pos 0 bd

        q ~= enqueue (cons (dep + 1) $ vadd pos v1) q
        q ~= enqueue (cons (dep + 1) $ vadd pos v2) q
        q ~= enqueue (cons (dep + 1) $ vadd pos v3) q
        q ~= enqueue (cons (dep + 1) $ vadd pos v4) q

    trace (c 77777, tick)
    e ret
-}

isJunction :: Expr Pos -> Expr Map -> Expr Int
(isJunction, isJunctionDef) = def2 "isJunction" $ \pos bd -> comp $ do
  with2 (car pos) (cdr pos) $ \x y -> e $
    (peekMat (x+1) y bd ./= 0) +
    (peekMat (x-1) y bd ./= 0) +
    (peekMat x (y+1) bd ./= 0) +
    (peekMat x (y-1) bd ./= 0) .>= 3

junctionSafety :: Expr Map -> Expr Map -> Expr Pos -> Expr Pos -> Expr Int
(junctionSafety, junctionSafetyDef) = def4 "junctionSafety" $ \bd ghostMap pos vect -> comp $ do
  with3 (undef :: Expr Int) (vadd pos vect) 1 $ \ret cur pac -> do
    while (peekMat (car cur) (cdr cur) bd ./= 0) $ do
      cond (isJunction cur bd)
        (do ret ~= peekMat (car cur) (cdr cur) ghostMap - pac
            cur ~= cons 0 0
        )
        (do cur ~= vadd cur vect
            pac ~= pac + 1
        )
    e ret

lmax :: Expr Int -> Expr Int -> Expr Int
(lmax, lmaxDef) = def2 "lmax" $ \a b ->
  ite (a .< b) a b

maxJunctionSafety :: Expr Map -> Expr Map -> Expr Pos -> Expr Int
maxJunctionSafety bd ghostMap pos = comp $ withVects $ \[v1, v2, v3, v4] -> e $
  lmax (junctionSafety bd ghostMap pos v1) $
  lmax (junctionSafety bd ghostMap pos v2) $
  lmax (junctionSafety bd ghostMap pos v3)
       (junctionSafety bd ghostMap pos v4)

mapGhostPos :: Expr [GhostState] -> Expr [Pos]
(mapGhostPos, mapGhostPosDef) = def1 "mapGhostPos" $ \xs ->
  ite (isNull xs) lnull $
  ite (car (lhead xs) .== 0) (lcons (cadr $ lhead xs) $ mapGhostPos $ ltail xs) $
  (mapGhostPos $ ltail xs)

mapEdGhostPos :: Expr [GhostState] -> Expr [Pos]
(mapEdGhostPos, mapEdGhostPosDef) = def1 "mapEdGhostPos" $ \xs ->
  ite (isNull xs) lnull $
  ite (car (lhead xs) .== 1) (lcons (cadr $ lhead xs) $ mapEdGhostPos $ ltail xs) $
  (mapEdGhostPos $ ltail xs)

selectMax :: Expr Map -> Expr Pos -> Expr Int -> Expr Int
selectMax bd pos def = comp $
  withVects $ \[v0, v1, v2, v3] ->
  with (peekMap (vadd pos v0) bd) $ \c0 ->
  with (peekMap (vadd pos v1) bd) $ \c1 ->
  with (peekMap (vadd pos v2) bd) $ \c2 ->
  with (peekMap (vadd pos v3) bd) $ \c3 ->
    cond (c0 ./= inf &&& c0 .>= c1 &&& c0 .>= c2 &&& c0 .>= c3) (e $ c 0) $
    cond (c1 ./= inf &&& c1 .>= c2 &&& c1 .>= c3) (e $ c 1) $
    cond (c2 ./= inf &&& c2 .>= c3) (e $ c 2) $
    cond (c3 ./= inf) (e $ c 3) (e def)

selectMin :: Expr Map -> Expr Pos -> Expr Int -> Expr Int
selectMin bd pos def = comp $
  withVects $ \[v0, v1, v2, v3] ->
  with (peekMap (vadd pos v0) bd) $ \c0 ->
  with (peekMap (vadd pos v1) bd) $ \c1 ->
  with (peekMap (vadd pos v2) bd) $ \c2 ->
  with (peekMap (vadd pos v3) bd) $ \c3 ->
    cond (c0 ./= inf &&& c0 .<= c1 &&& c0 .<= c2 &&& c0 .<= c3) (e $ c 0) $
    cond (c1 ./= inf &&& c1 .<= c2 &&& c1 .<= c3) (e $ c 1) $
    cond (c2 ./= inf &&& c2 .<= c3) (e $ c 2) $
    cond (c3 ./= inf) (e $ c 3) (e def)

-- inv
inv :: Expr Int -> Expr Int
(inv, invDef) = def1 "inv" $ \t -> ite (t .== inf) (-inf) t

selectMax' :: Expr Pos -> Expr Int -> (Expr Pos -> Expr Int) -> Expr Int
selectMax' pos def score = comp $
  withVects $ \[v0, v1, v2, v3] ->
  with (inv $ score $ vadd pos v0) $ \c0 ->
  with (inv $ score $ vadd pos v1) $ \c1 ->
  with (inv $ score $ vadd pos v2) $ \c2 ->
  with (inv $ score $ vadd pos v3) $ \c3 -> do
    trace (c 100007, c0, c1, c2, c3)

    cond (c0 ./= -inf &&& c0 .>= c1 &&& c0 .>= c2 &&& c0 .>= c3) (e $ c 0) $
      cond (c1 ./= -inf &&& c1 .>= c2 &&& c1 .>= c3) (e $ c 1) $
      cond (c2 ./= -inf &&& c2 .>= c3) (e $ c 2) $
      cond (c3 ./= -inf) (e $ c 3) (e def)

selectMin' :: Expr Pos -> Expr Int -> (Expr Pos -> Expr Int) -> Expr Int
selectMin' pos def score = comp $
  withVects $ \[v0, v1, v2, v3] ->
  with (score $ vadd pos v0) $ \c0 ->
  with (score $ vadd pos v1) $ \c1 ->
  with (score $ vadd pos v2) $ \c2 ->
  with (score $ vadd pos v3) $ \c3 -> do
    trace (c 100009, c0, c1, c2, c3)

    cond (c0 ./= inf &&& c0 .<= c1 &&& c0 .<= c2 &&& c0 .<= c3) (e $ c 0) $
      cond (c1 ./= inf &&& c1 .<= c2 &&& c1 .<= c3) (e $ c 1) $
      cond (c2 ./= inf &&& c2 .<= c3) (e $ c 2) $
      cond (c3 ./= inf) (e $ c 3) (e def)

anyValid :: Expr Map -> Expr Pos -> Expr Int
anyValid bd pos = comp $
  withVects $ \[v0, v1, v2, v3] ->
  with (peekMap (vadd pos v0) bd) $ \c0 ->
  with (peekMap (vadd pos v1) bd) $ \c1 ->
  with (peekMap (vadd pos v2) bd) $ \c2 ->
  with (peekMap (vadd pos v3) bd) $ \c3 -> e $
    ite (c0 ./= 0) (c 0) $
    ite (c1 ./= 0) (c 1) $
    ite (c2 ./= 0) (c 2) (c 3)

push :: Expr [a] -> Expr a -> CExpr () ()
push ls v = ls ~= lcons v ls

getDots :: Expr Map -> Expr ([Pos], [Pos])
(getDots, getDotsDef) = def1 "getDots" $ \bd -> comp $
  with4 lnull lnull (getWidth bd) (getHeight bd) $ \dot pow w h -> do
    for 0 h $ \y ->
      for 0 w $ \x ->
        with (peekMat x y bd) $ \cell -> do
          lwhen (cell .== 2) $ dot ~= lcons (cons x y) dot
          lwhen (cell .== 3) $ pow ~= lcons (cons x y) pow
    e $ cons dot pow

-- Strategy:
-- [1] if NearestGhost<3 then FromGhost+
-- [1] if MaxJunctionSafety>3 then FromGhost-
-- [2] if NearestEdGhost>99 then ToPowerDot+
-- [2] if NearestEdGhost<99 then ToEdGhost+
-- [2] if GhostDensity<1.5 and NearestPowerDot<5 then FromPowerDot+
-- [3] if Constant>0 then ToCenterofDots+

step :: Expr AIState -> Expr World -> Expr (AIState, Int)
(step, stepDef) = def2 "step" $ \bd world -> comp $
  with (cadr $ cadr world) $ \lmanPos -> do
    bd ~= pokeMap lmanPos 1 bd
    with (mapGhostPos $ caddr world) $ \ghosts ->
      with (mapEdGhostPos $ caddr world) $ \edGhosts ->
      -- with (getDots bd) $ \bothDots ->
      -- with2 (car bothDots) (cdr bothDots) $ \dots pows ->
      -- with (paint bd ghosts)   $ \ghostMap ->
      -- with (paint bd edGhosts) $ \edGhostMap ->
      -- with (paint bd dots) $ \dotMap ->
      -- with (paint bd pows) $ \powMap ->
      with (-1) $ \dir ->
      with undef $ \distToPill ->
      with undef $ \distToGhost ->
      with undef $ \distToEdGhost ->
      with (updPoss edGhosts 7 bd) $ \bdWithEdGhost ->
      with (updPoss ghosts 7 bd) $ \bdWithGhost -> do
        -- traceMap 678 bd
        -- traceMat 5675 dotMap

        distToGhost ~= bfs bdWithGhost lmanPos 7 4

        lwhen (distToGhost .< 4) $ do
          trace (c 30001)
          dir ~= selectMax' lmanPos dir (\p -> bfs bdWithGhost p 7 quota)

        lwhen (dir .== -1) $ do
          distToEdGhost ~= bfs bdWithEdGhost lmanPos 7 3
          lwhen (distToEdGhost .< 10) $ do
            trace (c 30002)
            dir ~= selectMin' lmanPos dir (\p -> bfs bdWithEdGhost p 7 quota)

        lwhen (dir .== -1 &&& isNull edGhosts) $ do
          trace (c 30003)
          dir ~= selectMin' lmanPos dir (\p -> bfs bd p 3 quota)

        lwhen (dir .== -1) $ do
          -- trace (c 20001, lmanPos)
          trace (c 30004)
          dir ~= selectMin' lmanPos dir (\p -> bfs bd p 2 quota)

--        dir ~= selectMin dotMap lmanPos dir
--        lwhen (peekMap lmanPos edGhostMap .>= inf &&& peekMap lmanPos powMap ./= inf) $
--          dir ~= selectMin powMap lmanPos dir

{-
        lwhen (peekMap lmanPos edGhostMap .<  inf) $
          dir ~= selectMin edGhostMap lmanPos dir

-}
--        lwhen (peekMap lmanPos ghostMap  .< 3) $
--          dir ~= selectMax ghostMap lmanPos dir

        lwhen (dir .== -1) $ do
          trace (c 30005)
          dir ~= anyValid bd lmanPos

{-
        trace (c 99998, dots)
        trace (c 99999, selectMin dotMap lmanPos, dir)

        trace (c 99996, dotMap)

        trace (c 10005, ghosts)
        trace (c 10006, edGhosts)
        trace (c 10001, peekMap lmanPos ghostMap)
        trace (c 10002, peekMap lmanPos edGhostMap)

        trace (c 99994, pows)

        traceMat 1  $ newMat 10 10 9
-}

        e $ cons bd dir

traceMap :: Int -> Expr Map -> CExpr () ()
traceMap tag m = trace (c 100000, Const tag, m)

traceMat :: Int -> Expr Map -> CExpr () ()
traceMat tag m = trace (c 100001, Const tag, m)

arrLength :: Expr (Array a) -> Expr Int
arrLength = car

matSize :: Expr (Mat a) -> Expr (Int, Int)
matSize m = cons (arrLength $ peek 0 m) (arrLength m)

initialize :: Expr World -> Expr X -> Expr AIState
(initialize, initializeDef) = def2 "initialize" $ \w _ -> comp $ do
  with4 (toMat (car w)) undef undef undef $ \mat sz w h -> do
    sz ~= matSize mat
    w ~= car sz
    h ~= cdr sz

    for 0 h $ \y ->
      for 0 w $ \x -> e $
        ite (peekMat x y mat .<= c 3) (c 0) $
          comp $ mat ~= pokeMat x y 1 mat

    e $ mat

progn :: LMan ()
progn = do
  libDef

  stepDef
  initializeDef

  bfsDef
  paintDef
  mapGhostPosDef
  mapEdGhostPosDef
  toQueueDef
  getDotsDef
  updPossDef
  invDef

{-
  emitComp $ do
    debug $ ldrop 2 (list [1,2,3,4,5] :: Expr [Int])
    debug $ ltake 2 (list [1,2,3,4,5] :: Expr [Int])

    with (mkArray $ list [1, 2, 3, 4, c 5]) $ \arr -> do
      debug arr

      for 0 5 $ \i ->
        debugn $ peek i arr

      for 0 5 $ \i -> do
        trace (c 11111, i)
        arr ~= poke i (peek i arr * peek i arr) arr

      debug arr

    with (newMat 22 23 $ c 0) $ \m -> do

      debugn 1234
      for 0 22 $ \i ->
        for 0 23 $ \j ->
        e $ peekMat i j m
      debugn 5678

      for 0 22 $ \i ->
        for 0 23 $ \j ->
        e $ pokeMat i j 777 m
      debugn 9999

    with (newArray (22*23) $ c 0) $ \m -> do
      debugn 11234
      for 0 22 $ \i ->
        for 0 23 $ \j ->
        e $ peek (i+22*j) m
      debugn 15678

      for 0 22 $ \i ->
        for 0 23 $ \j ->
        e $ poke (i+22*j) 777 m
      debugn 19999
-}

  -- rtn $ c 0

  rtn $ cons (initialize (Var (-1) 0) (Var (-1) 1)) (Closure "step")

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["debug"] -> do
      mapM_ putStrLn $ compile' progn
    _ -> do
      putStrLn $ compile progn

{-
progn :: LMan ()
progn = do
  libDef

  -- cexpr $ do
  --   cwith (mkArray (list $ map fromIntegral [0..9 :: Int] :: Expr [Int])) $ \arr -> do
  --     debug arr

  --     for 0 10 $ \i -> do
  --       debug $ cons i $ peek i arr

  --     for 0 10 $ \i -> do
  --       arr ~= poke i (peek i arr * peek i arr) arr

  --     debug arr

  --     for 0 10 $ \i -> do
  --       debug $ cons i $ peek i arr

  -- cexpr $ do
  --   debug $ lreverse $ list [1, 2, 3, 4, 5 :: Expr Int]
  --   debugn 1234

  --   cwith 123 $ \i -> do
  --     debugn i
  --     debugn $ i * 2
  --     i ~= i * i
  --     debugn i

  --   cwith 0 $ \i -> do
  --     while (i .< 10) $ comp $ do
  --       debugn i
  --       i ~= i + 1

  -- expr $ do
  --   with (enqueue 1 emptyQueue :: Expr (Queue Int)) $ \q1 ->
  --     with (enqueue 2 q1) $ \q2 ->
  --     with (enqueue 3 q2) $ \q3 ->
  --     with (dequeue q3) $ \r4 ->
  --     with (dequeue $ cdr r4) $ \r5 ->
  --     with (dequeue $ cdr r5) $ \r6 -> comp $ do
  --       debug q2
  --       debug q3
  --       debug r4
  --       debug r5
  --       debug r6
-}
