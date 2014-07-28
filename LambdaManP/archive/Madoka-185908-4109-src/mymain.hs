{-# LANGUAGE FlexibleInstances, GADTs #-}

import Control.Monad.RWS
import qualified Data.Map as M
import Data.Maybe
import Data.Char

type LMan = RWS () [String] CompState

data CompState
  = CompState
    { csUniq :: Int
    }

initState :: CompState
initState = CompState 0

data Label = Label String

newLabel :: LMan Label
newLabel = do
  s <- get
  let ret = csUniq s + 1
  put $ s { csUniq = ret }
  return $ Label $ "L" ++ show ret

data Expr a where
  Const :: Int -> Expr Int
  Var   :: Int -> Int -> Expr a
  Bin   :: Opr -> Expr a -> Expr a -> Expr a
  Cons  :: Expr a -> Expr b -> Expr (a, b)
  Car   :: Expr (a, b) -> Expr a
  Cdr   :: Expr (a, b) -> Expr b
  Ceq   :: Expr Int -> Expr Int -> Expr Int
  Cgt   :: Expr Int -> Expr Int -> Expr Int
  Cgte  :: Expr Int -> Expr Int -> Expr Int

data Opr
  = ADD
  | SUB
  | MUL
  | DIV
  deriving Show

instance Num (Expr Int) where
  a + b = Bin ADD a b
  a - b = Bin SUB a b
  a * b = Bin MUL a b

  abs = error "abs is not implemented"
  signum = error "signum is not implemented"

  fromInteger n = Const (fromInteger n)

instance Eq (Expr Int) where
  (==) = error "Eq does not supported"

instance Ord (Expr Int) where
  compare = error "Ord doed not supported"

instance Enum (Expr Int) where
  toEnum = error "Enum dows not supported"
  fromEnum = error "Enum dows not supported"

instance Real (Expr Int) where
  toRational = error "toRational does not supported"

instance Integral (Expr Int) where
  div = Bin DIV

  mod = error "mod dows not supported"

  quotRem = error "quotRem does not supported"
  toInteger = error "toInteger does not supported"

infix 4 .==, .<, .<=, .>, .>=

(.==) :: Expr Int -> Expr Int -> Expr Int
(.==) = Ceq

(.<) :: Expr Int -> Expr Int -> Expr Int
(.<) = flip Cgt

(.<=) :: Expr Int -> Expr Int -> Expr Int
(.<=) = flip Cgte

(.>) :: Expr Int -> Expr Int -> Expr Int
(.>) = Cgt

(.>=) :: Expr Int -> Expr Int -> Expr Int
(.>=) = Cgte

compileExpr :: Expr a -> LMan ()
compileExpr e = case e of
  Const n -> ldc n

  Var i j -> error "var is not implemented"

  Bin opr a b -> do
    compileExpr a
    compileExpr b
    tell [show opr]

  Cons a b -> do
    compileExpr a
    compileExpr b
    tell ["CONS"]

  Car a -> do
    compileExpr a
    tell ["CAR"]

  Cdr a -> do
    compileExpr a
    tell ["CDR"]

  Ceq a b -> do
    compileExpr a
    compileExpr b
    tell ["CEQ"]

  Cgt a b -> do
    compileExpr a
    compileExpr b
    tell ["CGT"]

  Cgte a b -> do
    compileExpr a
    compileExpr b
    tell ["CGTE"]

ldc :: Int -> LMan ()
ldc n = tell ["LDC " ++ show n]

ldf :: LMan () -> LMan ()
ldf = error "ldf is not implemented"

dbug :: Expr a -> LMan ()
dbug e = do
  compileExpr e
  tell ["DBUG"]

dbugn :: Expr Int -> LMan ()
dbugn = dbug

cons :: Expr a -> Expr b -> Expr (a, b)
cons = Cons

car :: Expr (a, b) -> Expr a
car = Car

cdr :: Expr (a, b) -> Expr b
cdr = Cdr

tsel :: Label -> Label -> LMan ()
tsel (Label t) (Label e) = do
  tell ["TSEL " ++ t ++ " " ++ e]

emitLabel :: Label -> LMan ()
emitLabel (Label l) = do
  tell [l ++ ":"]

ite :: Expr Int -> LMan () -> LMan () -> LMan ()
ite cond t e = do
  tlabel <- newLabel
  elabel <- newLabel
  jlabel <- newLabel
  compileExpr cond
  tsel tlabel elabel
  emitLabel tlabel
  t
  ldc 0
  tsel jlabel jlabel
  emitLabel elabel
  e
  emitLabel jlabel

codeGen :: LMan () -> [String]
codeGen p =
  let ((), codes) = evalRWS p () initState
  in codes

----

desugar :: [String] -> [String]
desugar s =
  let ls = filter (not . null . words) . map removeComment $ s
      (adr, codes) = collectAddress ls
      dest = map (subst adr) codes
  in dest

subst :: M.Map String Int -> String -> String
subst m s =
  case words s of
    (opc: opr) ->
      unwords $ opc : map f opr
  where
    f name
      | M.member name m = show $ fromJust $ M.lookup name m
      | all isDigit name = name
      | otherwise = error $ "undefined label: " ++ show name

collectAddress :: [String] -> (M.Map String Int, [String])
collectAddress = go 0 M.empty [] where
  go adr addrs acc [] = (addrs, reverse acc)
  go adr addrs acc (l:ls) =
    case getLabel l of
      Just name
        | M.member name addrs ->
          error $ "duplicate label: " ++ name
        | otherwise ->
          go adr (M.insert name adr addrs) acc ls
      Nothing ->
        go (adr+1) addrs (l:acc) ls

getLabel l =
  let l' = unwords $ words l
  in if last l' == ':' && all isLabelChar (init l') then Just $ init l' else Nothing

isLabelChar c = isAlpha c || isDigit c

removeComment = takeWhile (/= ';')

-----

compile :: LMan () -> String
compile = unlines . desugar . codeGen

-----

-- library



-----

progn :: LMan ()
progn = do
  {-
  dbugn (3 - 2)
  dbug  (cons (cons 1 2) 3 :: Expr ((Int, Int), Int))
  dbug  (cdr ((cons (cons 1 2) 3 :: Expr ((Int, Int), Int))))
  dbugn (42 `div` 5)
  -- dbug (42 `mod` 5 :: Expr Int)
  -}

  ite (0 .== 1)
    (dbugn 123)
    (dbugn 456)

  ite (0 .< 1)
    (dbugn 123)
    (dbugn 456)

  ite (0 .>= 1)
    (dbugn 123)
    (dbugn 456)

main :: IO ()
main = putStrLn $ compile progn
