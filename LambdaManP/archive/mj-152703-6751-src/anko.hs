{-# LANGUAGE FlexibleInstances, GADTs, RecursiveDo, ScopedTypeVariables, RecordWildCards, RankNTypes, ImpredicativeTypes, NoMonoLocalBinds #-}

import Data.Maybe
import System.Environment
import Control.Applicative (Const)

import DSL
import Lib
import Vect
import System.Random
import System.Process
import Text.Printf

import Prelude hiding (even, odd)


-----

type Map = Mat Int

data X = X

type World = ([[Int]], (ManState, ([GhostState], FruitState)))
type ManState = (Int, (Pos, (Direction, (Int, Int  ))))
type GhostState = (Int, (Pos , Direction))
type FruitState = Int

type AIState = (Map, ActionFlags)
type ActionFlags = Array Int

peekFlag :: ActionFlag -> Expr ActionFlags -> Expr Int
peekFlag f = peek (Const $ fromEnum f)
pokeFlag :: ActionFlag -> Expr Int -> Expr ActionFlags -> Expr ActionFlags 
pokeFlag f = poke (Const $ fromEnum f)

-- lowest array is of highest priority
data ActionFlag 
  = ToDot 
  | FromDot
  | ToPowerDot 
  | FromPowerDot 
  | ToGhost
  | FromGhost
  | ToCenterOfDots
  deriving (Eq, Ord, Enum,Bounded)


actionFlagSize :: Num a => a
actionFlagSize = fromIntegral $ 1+fromEnum (maxBound :: ActionFlag)


mapOfS :: Expr AIState -> Expr Map
mapOfS = car
actionFlagsOfS :: Expr AIState -> Expr ActionFlags
actionFlagsOfS = cdr

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

bfs :: Expr Map -> Expr [Pos] -> Expr Int -> Expr Int
(bfs, bfsDef) = def3 "bfs" $ \bd gs target -> comp $
  with3 0 (toQueue gs) undef $ \out q qq ->
  with undef $ \cell ->
  withVects $ \[v1, v2, v3, v4] -> do
    while (lnot $ isEmptyQueue q) $ do
      qq ~= dequeue q
      q ~= cdr qq

      let dep = car $ car qq
          pos = cdr $ car qq

      cell ~= peekMat (car pos) (cdr pos) bd

      -- trace (c 10002, dep, pos, cell)

      cond (cell .== target)
        ( do
             out ~= dep
             q ~= emptyQueue
        )
        ( cond((cell .== 0) ||| (cell .== 6)) (e $ c 0) $ do
             bd ~= pokeMat (car pos) (cdr pos) 0 bd
             q ~= enqueue (cons (dep + 1) $ vadd pos v1) q
             q ~= enqueue (cons (dep + 1) $ vadd pos v2) q
             q ~= enqueue (cons (dep + 1) $ vadd pos v3) q
             q ~= enqueue (cons (dep + 1) $ vadd pos v4) q
        )

    e out

inf :: Expr Int
inf = Const 999999

calcDensFrom :: Expr Pos -> Expr [Pos] -> Expr Int
(calcDensFrom , calcDensFromDef) = def2 "calcDensFrom" $ \origin poss -> comp $ do
  e $ ite (atom poss) (Const 0) $ comp $
      with (100 - (vnorm $ lhead poss `vsub` origin)) $
      \ r -> e $ (ite (r .< 0) 0 r) + calcDensFrom origin (ltail poss)




paint :: Expr Map -> Expr [Pos] -> Expr Map
(paint, paintDef) = def2 "paint" $ \bd starts -> comp $
  with3 (newMat (getWidth bd) (getHeight bd) inf) (toQueue starts) undef $ \ret q qq ->
  with undef $ \cell ->
  withVects $ \[v1, v2, v3, v4] -> do
    while (lnot $ isEmptyQueue q) $ do
      qq ~= dequeue q
      q ~= cdr qq

      let dep = car $ car qq
          pos = cdr $ car qq

      cell ~= peekMat (car pos) (cdr pos) bd

      cond (cell .== 0) (e $ c 0) $ do
        -- trace (c 10005, dep, pos, cell)

        ret  ~= pokeMat (car pos) (cdr pos) dep ret
        bd ~= pokeMat (car pos) (cdr pos) 0 bd
        q ~= enqueue (cons (dep + 1) $ vadd pos v1) q
        q ~= enqueue (cons (dep + 1) $ vadd pos v2) q
        q ~= enqueue (cons (dep + 1) $ vadd pos v3) q
        q ~= enqueue (cons (dep + 1) $ vadd pos v4) q

    e ret

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
  ite (car (lhead xs) ./= 2) (lcons (cadr $ lhead xs) $ mapGhostPos $ ltail xs) $
  (mapGhostPos $ ltail xs)

selectMax :: Expr Map -> Expr Pos -> Expr Int
selectMax bd pos = comp $
  withVects $ \[v0, v1, v2, v3] ->
  with (peekMap (vadd pos v0) bd) $ \c0 ->
  with (peekMap (vadd pos v1) bd) $ \c1 ->
  with (peekMap (vadd pos v2) bd) $ \c2 ->
  with (peekMap (vadd pos v3) bd) $ \c3 ->
    cond (c0 ./= inf &&& c0 .> c1 &&& c0 .> c2 &&& c0 .> c3) (e $ c 0) $
    cond (c1 ./= inf &&& c1 .> c2 &&& c1 .> c3 &&& c1 .> c0) (e $ c 1) $
    cond (c2 ./= inf &&& c2 .> c3 &&& c2 .> c0 &&& c2 .> c1) (e $ c 2) $
    cond (c3 ./= inf &&& c3 .> c0 &&& c3 .> c1 &&& c3 .> c2) (e $ c 3) $
    e $ c (-1)

selectMin :: Expr Map -> Expr Pos -> Expr Int
selectMin bd pos = comp $
  withVects $ \[v0, v1, v2, v3] ->
  with (peekMap (vadd pos v0) bd) $ \c0 ->
  with (peekMap (vadd pos v1) bd) $ \c1 ->
  with (peekMap (vadd pos v2) bd) $ \c2 ->
  with (peekMap (vadd pos v3) bd) $ \c3 ->
    cond (c0 ./= inf &&& c0 .< c1 &&& c0 .< c2 &&& c0 .< c3) (e $ c 0) $
    cond (c1 ./= inf &&& c1 .< c2 &&& c1 .< c3 &&& c1 .< c0) (e $ c 1) $
    cond (c2 ./= inf &&& c2 .< c3 &&& c2 .< c0 &&& c2 .< c1) (e $ c 2) $
    cond (c3 ./= inf &&& c3 .< c0 &&& c3 .< c1 &&& c3 .< c2) (e $ c 3) $
    e $ c (-1)

selectSmall :: Expr Map -> Expr Pos -> Expr Int
selectSmall bd pos = comp $
  withVects $ \[v0, v1, v2, v3] ->
  with (peekMap (vadd pos v0) bd) $ \c0 ->
  with (peekMap (vadd pos v1) bd) $ \c1 ->
  with (peekMap (vadd pos v2) bd) $ \c2 ->
  with (peekMap (vadd pos v3) bd) $ \c3 ->
    cond (c0 ./= inf &&& c0 .< c1 &&& c0 .< c2 &&& c0 .< c3) (e $ c 0) $
    cond (c1 ./= inf &&& c1 .< c2 &&& c1 .< c3) (e $ c 1) $
    cond (c2 ./= inf &&& c2 .< c3) (e $ c 2) $
    e $ c 3


voteMax :: Expr Map -> Expr Pos -> Expr V4
voteMax bd pos = comp $
  withVects $ \[v0, v1, v2, v3] ->
  with (peekMap (vadd pos v0) bd) $ \c0 ->
  with (peekMap (vadd pos v1) bd) $ \c1 ->
  with (peekMap (vadd pos v2) bd) $ \c2 ->
  with (peekMap (vadd pos v3) bd) $ \c3 -> e $
    let 
        elem0 = c0 ./= inf &&& c0 .>= c1 &&& c0 .>= c2 &&& c0 .>= c3 
        elem1 = c1 ./= inf &&& c1 .>= c0 &&& c1 .>= c2 &&& c1 .>= c3
        elem2 = c2 ./= inf &&& c2 .>= c1 &&& c2 .>= c2 &&& c2 .>= c3 
        elem3 = c3 ./= inf &&& c3 .>= c0 &&& c3 .>= c1 &&& c3 .>= c2
    in cons (cons elem0 elem1) (cons elem2 elem3)


voteMin :: Expr Map -> Expr Pos -> Expr V4
voteMin bd pos = comp $
  withVects $ \[v0, v1, v2, v3] ->
  with (peekMap (vadd pos v0) bd) $ \c0 ->
  with (peekMap (vadd pos v1) bd) $ \c1 ->
  with (peekMap (vadd pos v2) bd) $ \c2 ->
  with (peekMap (vadd pos v3) bd) $ \c3 -> e $
    let 
        elem0 = c0 ./= inf &&& c0 .<= c1 &&& c0 .<= c2 &&& c0 .<= c3 
        elem1 = c1 ./= inf &&& c1 .<= c0 &&& c1 .<= c2 &&& c1 .<= c3
        elem2 = c2 ./= inf &&& c2 .<= c1 &&& c2 .<= c2 &&& c2 .<= c3 
        elem3 = c3 ./= inf &&& c3 .<= c0 &&& c3 .<= c1 &&& c3 .<= c2
    in cons (cons elem0 elem1) (cons elem2 elem3)



push :: Expr [a] -> Expr a -> CExpr () ()
push ls v = ls ~= lcons v ls

getDots :: Expr Map -> Expr ([Pos], [Pos])
(getDots, getDotsDef) = def1 "getDots" $ \bd -> comp $
  with4 lnull lnull (getWidth bd) (getHeight bd) $ \dot pow w h -> do
    for 0 h $ \y ->
      for 0 w $ \x ->
      with (peekMat x y bd) $ \cell ->
      cond (cell .== 2) (push dot $ cons x y) $
      cond (cell .== 3) (push pow $ cons x y) $
      (e $ c 0)
    e $ cons dot pow

-- Strategy:
-- [1] if NearestGhost<3 then FromGhost+
-- X [1] if MaxJunctionSafety>3 then FromGhost-
-- [2] if NearestGhost>99 then ToPowerDot+
-- [2] if NearestGhost<99 then ToGhost+
-- [2] if GhostDensity<1.5 and NearestPowerDot<5 then FromPowerDot+
-- [3] if Constant>0 then ToCenterofDots+

step :: Expr AIState -> Expr World -> Expr (AIState, Int)
(step, stepDef) = def2 "step" $ \aist world -> comp $
  with4 (cadr world) (cadr $ cadr world)  (mapOfS aist) (actionFlagsOfS aist)$ 
   \lmanState lmanPos bd actionFlags -> do
    bd ~= pokeMap lmanPos 1 bd
    with (mapGhostPos $ caddr world) $ \ghosts ->
      with (getDots bd) $ \bothDots ->
      with2 (car bothDots) (cdr bothDots) $ \dots pows ->
      with (paint bd ghosts)   $ \ghostMap ->
      with (paint bd dots) $ \dotMap ->
      with (paint bd pows) $ \powMap -> 
      with2 (car lmanState .> 0) (caddr lmanState) $ \lmanIsPow lmanDir ->
      with (calcDensFrom lmanPos ghosts) $ \ghostDens -> 
      with vzero4 $ \dirVote -> do
            
        -- [1]
        lwhen (peekMap lmanPos ghostMap .< 3 &&& lnot lmanIsPow) $ 
          actionFlags ~= pokeFlag FromGhost 1 actionFlags 
        -- lwhen (peekMap lmanPos ghostMap .> 5 ||| lmanIsPow) $ 
        lwhen (maxJunctionSafety bd ghostMap lmanPos .> 3 ||| lmanIsPow) $ 
          actionFlags ~= pokeFlag FromGhost 0 actionFlags 
        -- [2]
        lwhen (lnot lmanIsPow)  $ do
          actionFlags ~= pokeFlag ToPowerDot 1 actionFlags 
          actionFlags ~= pokeFlag ToGhost 0 actionFlags                   
          actionFlags ~= pokeFlag FromPowerDot 0 actionFlags           
        lwhen (lmanIsPow)  $ do
          actionFlags ~= pokeFlag ToGhost 1 actionFlags         
          actionFlags ~= pokeFlag ToPowerDot 0 actionFlags 
          actionFlags ~= pokeFlag FromPowerDot 0 actionFlags           
        lwhen (ghostDens .<= 150 &&& peekMap lmanPos powMap .< 5)$ do
          actionFlags ~= pokeFlag FromPowerDot 1 actionFlags 
          actionFlags ~= pokeFlag ToPowerDot 0 actionFlags           
          actionFlags ~= pokeFlag ToGhost 0 actionFlags                   
        -- [3]
        actionFlags ~= pokeFlag ToDot 1 actionFlags 
        
        let chainAction :: Expr Int -> ActionFlag -> Expr V4 -> Expr V4
            chainAction w f v = (peekFlag f actionFlags * w) `vscale4` v
        
        dirVote ~= dirVote `vadd4` chainAction 4 FromGhost    (voteMax  ghostMap lmanPos) 
        dirVote ~= dirVote `vadd4` chainAction 2 ToPowerDot   (voteMin  powMap lmanPos) 
        dirVote ~= dirVote `vadd4` chainAction 2 ToGhost      (voteMin  ghostMap lmanPos)  
        dirVote ~= dirVote `vadd4` chainAction 2 FromPowerDot (voteMax  powMap lmanPos)               
        dirVote ~= dirVote `vadd4` chainAction 1 ToDot        (voteMin  dotMap lmanPos) 


        trace (c 6677, actionFlags)
        trace dirVote   
        
        let dir = maxIndex4 dirVote
        
        e $ cons (cons bd actionFlags) dir

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
    e $ cons mat (newArray actionFlagSize (Const 0) :: Expr (Array Int))

progn :: LMan ()
progn = do
  libDef

  stepDef
  initializeDef

  bfsDef
  paintDef
  mapGhostPosDef
  toQueueDef
  getDotsDef
  calcDensFromDef
  junctionSafetyDef
  isJunctionDef
  lmaxDef

  rtn $ cons (initialize (Var (-1) 0) (Var (-1) 1)) (Closure "step")

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["debug"] -> do
      mapM_ putStrLn $ compile' progn
    _ -> do
      idx <- randomRIO (0,10000:: Integer)
      dateStr <- readProcess "date" ["+%H%M%S"] ""
      let 
          body = printf "archive/mj-%s-%04d" dateStr2 idx
          fnGcc :: String
          fnGcc = body ++ ".gcc"
          fnDir :: String
          fnDir = body ++ "-src"
          dateStr2 = 
            map (\c -> if c==' ' then '-' else c) $
            unwords $ words $
            map (\c -> if c `elem` "0123456789" then c else ' ') dateStr

          progStr = compile progn

      writeFile "/dev/null" progStr
      writeFile fnGcc $ progStr
      system $ "mkdir -p " ++ fnDir
      system $ printf "cp *.hs %s/" fnDir
      return ()
