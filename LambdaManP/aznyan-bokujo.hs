{-# LANGUAGE FlexibleInstances, GADTs, RecursiveDo, ScopedTypeVariables, RecordWildCards, RankNTypes, ImpredicativeTypes #-}

import qualified Data.Map as M
import Data.Maybe
import Data.Char
import Data.List (intercalate)
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
import System.Process

-----
type X = Int
type Direction = Int
type World = ([[Int]], (ManState, ([GhostState], FruitState)))

--               vit                    lives score
type ManState = (Int, (Pos, (Direction, (Int, Int  ))))
type GhostState = (Int, (Pos , Direction))
type FruitState = Int
type AIState = X
type Pos = (Int, Int)

randLIO :: (Double,Double) -> IO Int
randLIO (lo,hi) = do
  lgr <- randomRIO (log lo, log hi)
  return $ round $ exp lgr

randDIO :: (Double,Double) -> IO Double
randDIO (lo,hi) = do
  lgr <- randomRIO (log lo, log hi)
  return $ exp lgr


{-# NOINLINE pillParam #-}
pillParam ::  Int -- default: 100
pillParam = 100 -- this is the unit of value
{-# NOINLINE powerPillParam #-}
powerPillParam ::  Int -- 2000
powerPillParam = pillParam * (unsafePerformIO $ randLIO (1,20) )
{-# NOINLINE ghostPillParam #-}
ghostPillParam ::  Int -- 5000
ghostPillParam = negate $  unsafePerformIO $ randLIO (500,50000) 
{-# NOINLINE ghostPillParamF #-}
ghostPillParamF ::  Int --10000
ghostPillParamF =  unsafePerformIO $ randLIO (1000,50000) 

{-# NOINLINE ghostAuraParamF #-}
ghostAuraParamF :: Int -- 1600
ghostAuraParamF = unsafePerformIO $ randLIO (160,60000) 
{-# NOINLINE ghostAuraParam #-}
ghostAuraParam :: Int -- 800
ghostAuraParam = negate $ unsafePerformIO $ randLIO (80,80000) 


(tileValue :: Expr Int -> Expr Int, tileValueDef) = def1 "tileView" $ \i ->
  ite (i.==0) 0 $ 
  ite (i.==2) (Const pillParam) $ 
  ite (i.==3) (Const powerPillParam) $ 
  1


int_min :: Expr Int
int_min = Const $ -2^(31)

mapAt :: Expr Pos -> Expr [[Int]] -> Expr Int
mapAt pos chizu = nth (car pos) $ nth (cdr pos) chizu

veq :: Expr Pos -> Expr Pos -> Expr Int
veq  pos vect = (car pos .== car vect) * (cdr pos .== cdr vect)


vadd :: Expr Pos -> Expr Pos -> Expr Pos
vadd pos vect = cons (car pos + car vect) (cdr pos + cdr vect)

vsub :: Expr Pos -> Expr Pos -> Expr Pos
vsub pos vect = cons (car pos - car vect) (cdr pos - cdr vect)

vinner :: Expr Pos -> Expr Pos -> Expr Int
vinner pos vect = (car pos * car vect) + (cdr pos * cdr vect)

vrotR :: Expr Pos -> Expr Pos
vrotR vect = cons (negate $ cdr vect) (car vect)

vrotL :: Expr Pos -> Expr Pos
vrotL vect = cons (cdr vect) (negate $ car vect)

(dirValuePill:: Expr Int -> Expr Pos -> Expr World -> Expr Pos -> Expr Int, dirValuePillDef) =
  def4 "dirValuePill" $ \depth vect world manp ->
    let info = (mapAt manp chizu) 
        chizu :: Expr [[Int]]
        chizu = car world
        gss :: Expr [GhostState]
        gss = car $ cdr $ cdr world
        
        manState :: Expr ManState
        manState = car $ cdr world
        
        powerPillFlag :: Expr Int
        powerPillFlag = car manState

        subScore = (dirValuePill (depth+1) (vrotR vect) world (vsub manp vect)
                 + dirValuePill (depth+1) (vrotL vect) world (vsub manp vect)) `div` 2
        ghostVal :: Expr Int
        ghostVal = (ite powerPillFlag (Const ghostPillParamF) (Const ghostPillParam))
    in 
    ite (depth .>= 3) 0 $
    ite (info .== 0) subScore $
    ite (isGhostThere gss manp) ghostVal
    (tileValue info) + (dirValuePill depth vect world $ vadd manp vect)*9`div`10

(dirValueGhost1 :: Expr Pos -> Expr GhostState -> Expr Int -> Expr Pos -> Expr Int, dirValueGhost1Def) = 
  def4 "dirValueGhost1" $ \vect gs1 ppflag manp ->
    let ghostp :: Expr Pos 
        ghostp = car $ cdr gs1 
        
        vecDiff = vsub ghostp manp
        dist2 :: Expr Int
        dist2 = 1+vinner vecDiff vecDiff
        
    in ite ppflag ((Const ghostAuraParamF * vinner vect vecDiff) `div` dist2)
                  ((Const ghostAuraParam * vinner vect vecDiff) `div` dist2)


(dirValueGhosts :: Expr Pos -> Expr [GhostState] -> Expr Int -> Expr Pos -> Expr Int, dirValueGhostsDef) = 
  def4 "dirValueGhosts" $ \vect gss ppflag manp ->
    ite (atom gss) 0 $ dirValueGhost1 vect (lhead gss) ppflag manp + 
                       dirValueGhosts vect (ltail gss) ppflag manp  


(isGhostThere :: Expr [GhostState] -> Expr Pos -> Expr Int, isGhostThereDef) = 
  def2 "isGhostThere" $ \gss pos -> 
  let gs1 = lhead gss
      ghostPos = car $ cdr gs1
      ghostVit = car gs1      
      
  in ite (atom gss) 0 $ 
     ite ((ghostPos `veq` pos) - (ghostVit .== 2) .== 1) 1 $ 
     isGhostThere (ltail gss) pos



(dirValueTotal:: Expr Pos -> Expr World -> Expr Int, dirValueTotalDef) =
  def2 "dirValueTotal" $ \vect world -> 
    let manP :: Expr Pos
        manP = car $ cdr $ manState

        manState :: Expr ManState
        manState = car $ cdr world

        powerPillFlag :: Expr Int
        powerPillFlag = car manState
   
        chizu :: Expr [[Int]]
        chizu = car world
        
        gss :: Expr [GhostState]
        gss = car $ cdr $ cdr world
        
        nextP :: Expr Pos
        nextP = vadd manP vect
    in 
        ite (mapAt nextP chizu .== 0) int_min $
        dirValuePill 0 vect world manP 
          + dirValueGhosts vect gss powerPillFlag manP


(step :: Expr AIState -> Expr World -> Expr (AIState,Int), stepDef) =
  def2 "step" $ \aist world ->
    let scoreN, scoreE, scoreS, scoreW :: Expr Int
        scoreN = dirValueTotal (cons 0 (-1)) world
        scoreE = dirValueTotal (cons 1    0) world
        scoreS = dirValueTotal (cons 0    1) world
        scoreW = dirValueTotal (cons (-1) 0) world

        d2 :: Expr Int
        d2 = ite ((scoreN .>= scoreE) + (scoreN .>= scoreS) + (scoreN .>= scoreW) .== 3) 0 $
             ite ((scoreE .>= scoreS) + (scoreE .>= scoreW) .== 2) 1 $
             ite (scoreS .>= scoreW) 2 $
             (3 :: Expr Int)

    in dbug (list [scoreN, scoreE, scoreS, scoreW])
        `Seq` cons aist d2

progn :: LMan ()
progn = do
  nthDef
  tileValueDef
  dirValuePillDef
  dirValueGhost1Def  
  dirValueGhostsDef  
  dirValueTotalDef
  isGhostThereDef
  stepDef

  expr $ cons (0 :: Expr AIState) $ Closure "step"

data TestConf = TestConf 
  {ghostFiles::[String], 
   lambdaManFile :: String,
   mapFile :: String,
   scoreResult :: Int}
  deriving (Eq, Ord)


cmdlineOpts :: TestConf -> [String]
cmdlineOpts tc = 
  [ printf "--map=%s" (mapFile tc) 
  , printf "--ghost=%s" (intercalate "," $ ghostFiles tc) 
  , printf "--lambda=%s" (lambdaManFile tc)]

toCmdlineString :: TestConf -> String
toCmdlineString tc = unwords $ "./sim.sh" :  cmdlineOpts tc


ppTestConf :: TestConf -> String
ppTestConf tc = (show $ scoreResult tc) ++"\t" ++ toCmdlineString tc

mkTestConfs :: String -> [TestConf]
mkTestConfs gccfn = do -- List Monad
--  mapOpt <- ["map/train-1.map", "map/train-2.map", "map/train-3.map"]
  mapOpt <- ["map/kichiku.map"]
  gOpt <-
    [ ["ghost/chase_with_random.ghc","ghost/scatter.ghc","ghost/random_and_chase.ghc"]
    , ["ghost/chase_with_random.ghc","ghost/scatter.ghc","ghost/random_and_chase.ghc"]
    , ["ghost/chase_with_random.ghc","ghost/scatter.ghc","ghost/random_and_chase.ghc"]]


  return $ TestConf {ghostFiles = gOpt, lambdaManFile = gccfn,
                     mapFile = mapOpt, scoreResult = -1}

readLastLine :: Handle -> Int -> IO Int
readLastLine h cand = do
  b <- hIsEOF h
  case b of
    True -> return cand
    False -> do
      l <- hGetLine h
      let mc = readMay l
      case mc of
        Just i -> readLastLine h i
        Nothing -> readLastLine h cand



modifyMapContent :: String -> IO String
modifyMapContent con0 = do
  pf <- randDIO (0.1,1::Double)
  gf <- randDIO (0.1,1::Double)  
  ppf <- randDIO (0.1,1::Double)    
  go pf gf ppf con0

  where 
    
    skim :: Char -> Double -> String -> IO String
    skim tgt f = mapM (skim1 tgt f)
    skim1 :: Char -> Double -> Char -> IO Char
    skim1 tgt f c
      | c/=tgt = return c
      | c==tgt = do
        p <- randomRIO (0,1)
        return $ if f<p then ' ' else c
    go  pf gf ppf con0 = do
      con1 <- skim '.' pf =<< 
          skim '=' gf =<< 
          skim 'o' ppf con0
  
      case '.' `elem` con1 &&  '=' `elem` con1 &&  'o' `elem` con1 of
        True -> return con1
        False -> go  pf gf ppf con0

performTest :: TestConf -> IO TestConf
performTest tc0 = do
  indexR <- randomRIO (0,2^30::Int)
  let mapFn0 = mapFile tc0
      (fnB, _)  = splitExtension mapFn0
      mapFn1 = printf "%s-%010d.map" fnB indexR
      (fnLM, _)  = splitExtension (lambdaManFile tc0)
      statFn1 = printf "%s-%010d.stat" fnLM indexR      
      tc1 = tc0 {mapFile = mapFn1}
  mapContent1 <- readFile mapFn0 >>= modifyMapContent
  
  writeFile mapFn1 mapContent1
  
  (_, Just hout, _, _) <-
      createProcess (proc "./sim.sh" (cmdlineOpts tc1) )
        { std_out = CreatePipe }
  score <- readLastLine hout 0
  let 
      ret = tc1{scoreResult = score} 
  hPutStrLn stderr $ ppTestConf ret

  let msg0 = unwords
        [(show $ scoreResult ret),
         "p" ,show pillParam, 
         "P" ,show powerPillParam,
         "gp",show ghostPillParam,   
         "fp",show ghostPillParamF,   
         "ga",show ghostAuraParam,   
         "fa",show ghostAuraParamF,
         "denp", show $ charFraction '.' mapContent1,
         "denP", show $ charFraction 'o' mapContent1,
         "denG", show $ charFraction '=' mapContent1 
          ] 
      msg1 = toCmdlineString tc1
      msg = unlines [msg0,msg1]
  writeFile statFn1 $ msg
  return $ ret

charFraction :: Char -> String -> Double
charFraction c s = (fromIntegral $ length $ filter (==c) s) / (fromIntegral $ length s)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["debug"] -> do
      mapM_ putStrLn $ compile' progn
    _ -> main2
    
main2 :: IO ()
main2 = do
  indexR <-randomRIO (0,2^31 :: Int)
  let indexStr :: String
      indexStr = printf "%010d" indexR
      gccFn :: String
      txtFn :: String
      gccFn = printf "./LambdaMan/gen2/az-%s.gcc" indexStr
      txtFn = printf "./LambdaMan/gen2/az-%s.txt" indexStr
      logFn = printf "./LambdaMan/gen2/az-%s.log" indexStr      

  writeFile gccFn $ compile progn
  let tcs0 = mkTestConfs gccFn
  print $ length tcs0
  
  tcs <- mapM performTest tcs0
  
  writeFile logFn $ unlines $ map ppTestConf tcs
  let msg = unwords
        [(show $ sum $ map scoreResult tcs), "/", (show $ length tcs),
         "p" ,show pillParam, 
         "P" ,show powerPillParam,
         "gp",show ghostPillParam,   
         "fp",show ghostPillParamF,   
         "ga",show ghostAuraParam,   
         "fa",show ghostAuraParamF
          ] ++ "\n"
  writeFile txtFn $ msg
  putStr $ msg
  
