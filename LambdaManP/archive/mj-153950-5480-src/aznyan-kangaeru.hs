{-# LANGUAGE FlexibleInstances, GADTs, RecursiveDo, ScopedTypeVariables, RecordWildCards, RankNTypes, ImpredicativeTypes #-}

import qualified Data.Map as M
import Data.Maybe
import Data.Char
import Data.Function (on)
import Data.List (intercalate, sortBy)
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

sotaMode :: Bool
sotaMode = unsafePerformIO $ do
  args <- getArgs
  return $ "sota" `elem` args

-----
type X = Int
type Direction = Int
type Clock = Int
type World0 = ([[Int]], (ManState, ([GhostState], FruitState)))
type World = ((Mat Int), (ManState, ([GhostState], FruitState)))

--               vit                    lives score
type ManState = (Int, (Pos, (Direction, (Int, Int  ))))
type GhostState = (Int, (Pos , Direction))
type FruitState = Int
type AIState = ((Mat Int),(Clock,X))
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
powerPillParam 
  | sotaMode  = 500
  | otherwise = pillParam * (unsafePerformIO $ randLIO (1,20) )
{-# NOINLINE ghostPillParam #-}
ghostPillParam ::  Int -- 5000
ghostPillParam  
  | sotaMode  = -925
  | otherwise = negate $  unsafePerformIO $ randLIO (500,50000) 
{-# NOINLINE ghostPillParamF #-}
ghostPillParamF ::  Int --10000
ghostPillParamF 
  | sotaMode  = 8080
  | otherwise =  unsafePerformIO $ randLIO (1000,50000) 

{-# NOINLINE ghostAuraParamF #-}
ghostAuraParamF :: Int -- 1600
ghostAuraParamF 
  | sotaMode  = -200
  | otherwise = unsafePerformIO $ randLIO (160,60000) 
{-# NOINLINE ghostAuraParam #-}
ghostAuraParam :: Int -- 800
ghostAuraParam 
  | sotaMode  = 800
  | otherwise = negate $ unsafePerformIO $ randLIO (80,80000) 

{-# NOINLINE dampingParam #-}
dampingParam :: Int -- 800
dampingParam 
  | sotaMode  = 90
  | otherwise = 100 + (negate $ unsafePerformIO $ randLIO (1,50) )



(tileValue :: Expr Int -> Expr Int -> Expr Int, tileValueDef) = def2 "tileView" $ \i ppFlag ->
  ite (i.==0) 0 $ 
  ite (i.==2) (ite ppFlag 0 $ Const pillParam) $ 
  ite (i.==3) (Const powerPillParam) $ 
  1


int_min :: Expr Int
int_min = Const $ -2^(31)

mapAt :: Expr Pos -> Expr (Mat Int) -> Expr Int
mapAt pos chizu = peekMat (car pos) (cdr pos) chizu

mapPoke :: Expr Pos -> Expr Int -> Expr (Mat Int) -> Expr (Mat Int)
mapPoke pos val chizu = pokeMat (car pos) (cdr pos) val chizu

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
  def4 "dirValuePill" $ \juice vect world manp ->
    let info = (mapAt manp chizu) 
        chizu :: Expr (Mat Int)
        chizu = car world
        gss :: Expr [GhostState]
        gss = car $ cdr $ cdr world
        
        manState :: Expr ManState
        manState = car $ cdr world
        
        powerPillFlag :: Expr Int
        powerPillFlag = car manState

        subScore = (dirValuePill (div juice 2) (vrotR vect) world (vsub manp vect)
                 + dirValuePill (div juice 2) (vrotL vect) world (vsub manp vect)) `div` 2
        ghostVal :: Expr Int
        ghostVal = (ite powerPillFlag (Const ghostPillParamF) (Const ghostPillParam))
    in 
    ite (juice .<= 0) 0 $
    ite (info .== 0) subScore $
    ite (isGhostThere gss manp) ghostVal
    (tileValue powerPillFlag info) + (dirValuePill (div juice 2) vect world $ vadd manp vect)* (Const dampingParam)`div`100

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
   
        chizu :: Expr (Mat Int)
        chizu = car world
        
        gss :: Expr [GhostState]
        gss = car $ cdr $ cdr world
        
        nextP :: Expr Pos
        nextP = vadd manP vect
    in 
        ite (mapAt nextP chizu .== 0) int_min $
        dirValuePill 100 vect world manP 
          + dirValueGhosts vect gss powerPillFlag manP
          + mapAt nextP chizu


(step :: Expr AIState -> Expr World0 -> Expr (AIState,Int), stepDef) =
  def2 "step" $ \aist world0 -> with (car aist) $ \chizu -> 
    let world = cons chizu (cdr world0)
        clk :: Expr Clock
        clk = car $ cdr aist
        scoreN, scoreE, scoreS, scoreW :: Expr Int
        scoreN = dirValueTotal (cons 0 (-1)) world
        scoreE = dirValueTotal (cons 1    0) world
        scoreS = dirValueTotal (cons 0    1) world
        scoreW = dirValueTotal (cons (-1) 0) world

        d2 :: Expr Int
        d2 = ite ((scoreN .>= scoreE) + (scoreN .>= scoreS) + (scoreN .>= scoreW) .== 3) 0 $
             ite ((scoreE .>= scoreS) + (scoreE .>= scoreW) .== 2) 1 $
             ite (scoreS .>= scoreW) 2 $
             (3 :: Expr Int)

        manP :: Expr Pos
        manP = car $ cdr $ manState

        manState :: Expr ManState
        manState = car $ cdr world

        newAist :: Expr AIState
        newAist = cons (mapPoke manP (negate clk) chizu) $
                  cons (clk+1) (cdr $cdr aist)
    in dbug (list [scoreN, scoreE, scoreS, scoreW]) `Seq`
--       dbug chizu `Seq`
       cons newAist d2

progn :: LMan ()
progn = do
  tileValueDef
  dirValuePillDef
  dirValueGhost1Def  
  dirValueGhostsDef  
  dirValueTotalDef
  isGhostThereDef
  stepDef
  
  libDef

  -- emitComp $ debug 
  
  let chizu = toMat $ car (Var (-1) 0 :: Expr World0)
  
  rtn $ cons (cons chizu (cons 0 0) :: Expr AIState) $ Closure "step"

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

randChoice1 :: [a] -> IO a
randChoice1 xs = do
  idx <- randomRIO (0, length xs)
  return $ xs!!idx

randChoiceN :: Int -> [a] -> IO [a]
randChoiceN n xs = do
  xs2 <- mapM pairWith xs 
  let xs3 = sortBy (compare `on` fst) xs2
  return $ map snd $ take n xs3

  where 
    pairWith :: a -> IO (Double, a)
    pairWith x = do
      k <- randomRIO (0,1)
      return (k,x)

mkTestConfs :: String -> IO [TestConf]
mkTestConfs gccfn = do 
  mapOpts <- randChoiceN 3 ["map/train-1.map", "map/train-2.map", "map/train-3.map", "map/train-4.map", "map/train-5.map" ]
  let gOpts =
        [ ["ghost/chase_with_random.ghc","ghost/scatter.ghc","ghost/random_and_chase.ghc"]
        , ["ghost/chase_with_random.ghc","ghost/scatter.ghc","ghost/chase_fixed.ghc"]
        , ["ghost/chase_fixed.ghc"]]


  return $ do -- listMonad
    gOpt <- gOpts
    mapOpt <- mapOpts
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
      mapFn1 = printf "%s-%010d.map" fnLM indexR
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
         "damp" ,show dampingParam, 
         "Np", show $ charCnt '.' mapContent1,
         "NP", show $ charCnt 'o' mapContent1,
         "NG", show $ charCnt '=' mapContent1,
         "N", show $ length mapContent1,
	  "# " ++ toCmdlineString tc1 
          ] 
      msg = unlines [msg0]
  writeFile statFn1 $ msg
  return $ ret


    where
     charCnt :: Char -> String -> Int
     charCnt c s =  length $ filter (==c) s

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["debug"] -> do
      mapM_ putStrLn $ compile' progn
    ["sota"] -> writeFile "aznyan-sota.gcc" $ compile progn
    _        -> main2
    
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
  tcs0 <- mkTestConfs gccFn
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
         "fa",show ghostAuraParamF,
         "damp" ,show dampingParam           
         ] ++ "\n"
  writeFile txtFn $ msg
  putStr $ msg
  
