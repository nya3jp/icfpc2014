{-# LANGUAGE FlexibleInstances, GADTs, RecursiveDo, ScopedTypeVariables, Rank2Types, LiberalTypeSynonyms, ImpredicativeTypes #-}

module DSL where

import Control.Monad.RWS hiding (local)
import Control.Monad.State
import Unsafe.Coerce

import Desugar

type LMan = RWS () [String] CompState

data CompState
  = CompState
    { csUniq :: Int
    , csEnvLevel :: Int
    }

initState :: CompState
initState = CompState 0 0

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
  Dbug  :: Expr a -> Expr ()

  Lnull :: Expr [a]
  Lcons :: Expr a -> Expr [a] -> Expr [a]

  Ite :: Expr Int -> Expr a -> Expr a -> Expr a
  With :: Expr a -> (Expr a -> Expr r) -> Expr r
  Assign :: Int -> Int -> Expr a -> Expr ()

  Seq :: Expr a -> Expr b -> Expr b

  Closure :: Int -> Int -> Expr f

  Call1 :: Expr (a1 -> r) -> Expr a1 -> Expr r
  Call2 :: Expr (a1 -> a2 -> r) -> Expr a1 -> Expr a2 -> Expr r

-- data Any = forall a . Any a

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

  Var i j -> ld i j
  Closure i j -> ld i j

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

  Dbug e -> do
    compileExpr e
    tell ["DBUG"]

  Seq a b -> do
    compileExpr a
    compileExpr b

  Ite cond t e -> do
    tlabel <- newLabel
    elabel <- newLabel
    jlabel <- newLabel
    compileExpr cond
    tsel tlabel elabel
    emitLabel tlabel
    compileExpr t
    jmp jlabel
    emitLabel elabel
    compileExpr e
    emitLabel jlabel

  Assign i j e -> do
    compileExpr e
    lev <- gets csEnvLevel
    tell ["ST  " ++ show (lev - i) ++ " " ++ show j]

  With v f -> do
    l <- newLabel
    end <- newLabel
    compileExpr v
    ldf l
    tell ["AP 1"]
    jmp end

    emitLabel l
    block $ do
      v <- innerVar 0
      compileExpr $ f v
      tell ["RTN"]

    emitLabel end

  Call1 (Closure i j) v -> do
    compileExpr v
    ld i j
    tell ["AP 1"]

  Call2 (Closure i j) v1 v2 -> do
    compileExpr v1
    compileExpr v2
    ld i j
    tell ["AP 2"]

incrLevel :: LMan ()
incrLevel = do
  s <- get
  put $ s { csEnvLevel = csEnvLevel s + 1 }

block :: LMan a -> LMan a
block b = do
  s <- get
  r <- b
  t <- get
  put $ s { csUniq = csUniq t }
  return r

local :: LMan a -> LMan a
local b = block $ do
  incrLevel
  b

innerVar :: Int -> LMan (Expr a)
innerVar i = do
  s <- get
  return $ Var (csEnvLevel s) i

ld :: Int -> Int -> LMan ()
ld i j = do
  lev <- gets csEnvLevel
  tell ["LD " ++ show (lev - i) ++ " " ++ show j]

ldc :: Int -> LMan ()
ldc n = tell ["LDC " ++ show n]

ldf :: Label -> LMan ()
ldf (Label l) = tell ["LDF " ++ l]

dbug :: Expr a -> Expr ()
dbug e = Dbug e

dbugn :: Expr Int -> Expr ()
dbugn = dbug

cons :: Expr a -> Expr b -> Expr (a, b)
cons = Cons

car :: Expr (a, b) -> Expr a
car = Car

cdr :: Expr (a, b) -> Expr b
cdr = Cdr

lnull :: Expr [a]
lnull = unsafeCoerce $ Const 0

lcons :: Expr a -> Expr [a] -> Expr [a]
lcons = unsafeCoerce $ Cons

list :: [Expr a] -> Expr [a]
list = foldr lcons lnull

lhead :: Expr [a] -> Expr a
lhead = unsafeCoerce Car

ltail :: Expr [a] -> Expr [a]
ltail = unsafeCoerce Cdr

tsel :: Label -> Label -> LMan ()
tsel (Label t) (Label e) = do
  tell ["TSEL " ++ t ++ " " ++ e]

jmp :: Label -> LMan ()
jmp l = do
  tell ["LDC 0"]
  tsel l l

emitLabel :: Label -> LMan ()
emitLabel (Label l) = do
  tell [l ++ ":"]

ite :: Expr Int -> Expr a -> Expr a -> Expr a
ite = Ite

(~=) :: Expr a -> Expr a -> Expr ()
(Var i j) ~= v = Assign i j v
_ ~= _ = error $ "Left hand side of := must be variable"

codeGen :: LMan () -> [String]
codeGen p =
  let ((), codes) = evalRWS p () initState
  in codes

-----

with :: Expr a -> (Expr a -> Expr r)  -> Expr r
with = With

-----

footer :: LMan ()
footer = do
  tell ["RTN"]

compile :: LMan () -> String
compile = unlines . desugar . codeGen . (>> footer)

compile' :: LMan () -> String
compile' = unlines . map f.  codeGen . (>> footer) where
  f s
    | last s == ':' = s
    | otherwise = "  " ++ s

-----

expr :: Expr a -> LMan ()
expr e = compileExpr e

{-
val :: Expr a -> LMan (Expr a)
val e = do
  l <- newLabel
 end <- newLabel

  compileExpr v
  ldf l
  tell ["AP 1"]
  jmp end

  emitLabel l
  incrLevel
  v <- innerVar 0
  tell ["RTN"]

  emitLabel end
-}

tests = do
  -- dbugn (3 - 2)
  {-
  dbug  (cons (cons 1 2) 3 :: Expr ((Int, Int), Int))
  dbug  (cdr ((cons (cons 1 2) 3 :: Expr ((Int, Int), Int))))
  dbugn (42 `div` 5)
  -- dbug (42 `mod` 5 :: Expr Int)
  expr $ dbugn $ ite (0 .== 1) 123 456
  -- dbugn $ ite (0 .<  1) 123 456
  -- dbugn $ ite (0 .>= 1) 123 456

  expr $ with (1+2) $ \i ->
    dbugn (i*i) `Seq`
    dbugn (i*i*i)

  rec
    (fact :: Fun (Int -> Int)) <- fun1 $ \i ->
      ite (i .== 0) 1 (i * call1 fact (i - 1))

    (mod :: Fun (Int -> Int -> Int)) <- fun2 $ \i j ->
      i - i `div` j * j

  -- foo <- fun1 $ \i -> i + i * i

  expr $ dbugn $ call1 fact 10

  expr $ dbugn $ call2 mod 42 8

  return ()
  -}

  -- dbug $ call hoge 1 2 (cons 1 2)
  undefined

fun1 :: (Expr a1 -> Expr r) -> LMan (Expr (a1 -> r))
fun1 f = do
  fun <- newLabel
  clo <- newLabel
  end <- newLabel

  tell ["DUM 1"]
  incrLevel

  jmp clo

  emitLabel fun
  local $ do
    a <- innerVar 0
    compileExpr $ f a
    tell ["RTN"]

  emitLabel clo
  ldf fun
  ldf end
  tell ["TRAP 1"]

  emitLabel end
  lev <- gets csEnvLevel
  return $ Closure lev 0

fun2 :: (Expr a1 -> Expr a2 -> Expr r) -> LMan (Expr (a1 -> a2 -> r))
fun2 f = do
  fun <- newLabel
  clo <- newLabel
  end <- newLabel

  tell ["DUM 1"]
  incrLevel

  jmp clo

  emitLabel fun
  local $ do
    a1 <- innerVar 0
    a2 <- innerVar 1
    compileExpr $ f a1 a2
    tell ["RTN"]

  emitLabel clo
  ldf fun
  ldf end
  tell ["TRAP 1"]

  emitLabel end
  lev <- gets csEnvLevel
  return $ Closure lev 0

call1 :: Expr (a1 -> r) -> Expr a1 -> Expr r
call1 = Call1

call2 :: Expr (a1 -> a2 -> r) -> Expr a1 -> Expr a2 -> Expr r
call2 = Call2

-----

-- Library

{-
data Lib where
  Lib :: { nth :: Expr ([a] -> Int -> a) } -> Lib

lib :: LMan Li
lib = do
  rec
    nth' <- fun2 $ \xs i ->
      ite (i .== 0)
        (lhead xs)
        (call2 nth' (ltail xs) (i - 1))

  return $ Lib { nth = nth' }
-}

nth' :: LMan (forall a. Expr ([a] -> Int -> a))
nth' = do
  rec
    f <- fun2 $ \xs i ->
      ite (i .== 0)
        (lhead xs)
        (call2 f (ltail xs) (i - 1))
  return $ unsafeCoerce f