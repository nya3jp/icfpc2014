{-# LANGUAGE FlexibleInstances, GADTs, RecursiveDo, ScopedTypeVariables, Rank2Types, LiberalTypeSynonyms, ImpredicativeTypes, NoMonoLocalBinds #-}

module DSL where

import Control.Monad.RWS hiding (local, Any)
import Control.Monad.State
import Control.Monad.Writer hiding (Any)
import Data.Maybe
import Debug.Trace
import Unsafe.Coerce

import Desugar

type Env = [String]

type LMan = RWS () [Env -> String] CompState

data CompState
  = CompState
    { csUniq :: Int
    , csEnvLevel :: Int
    , csFuncs :: [(String, Label)]
    }

initState :: CompState
initState = CompState 0 0 []

addFunc :: String -> Label -> LMan ()
addFunc name l = do
  s <- get
  put $ s { csFuncs = (name, l) : csFuncs s }

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
  Atom  :: Expr a -> Expr Int
  Dbug  :: Expr a -> Expr ()
  Nop   :: Expr ()

  Lnull :: Expr [a]
  Lcons :: Expr a -> Expr [a] -> Expr [a]

  Ite :: Expr Int -> Expr a -> Expr a -> Expr a
  With :: Expr a -> (Expr a -> Expr r) -> Expr r
  Assign :: Int -> Int -> Expr a -> Expr ()

  Seq :: Expr a -> Expr b -> Expr b
  While :: Expr Int -> Expr a -> Expr ()

  Call1 :: String -> Expr a1 -> Expr r
  Call2 :: String -> Expr a1 -> Expr a2 -> Expr r
  Call3 :: String -> Expr a1 -> Expr a2 -> Expr a3 -> Expr r
  Call4 :: String -> Expr a1 -> Expr a2 -> Expr a3 -> Expr a4 -> Expr r

  Closure :: String -> Expr a

data Any = Any (forall a. Expr a)

type CExpr = Writer [Any]

e :: Expr a -> CExpr ()
e x = tell [Any $ unsafeCoerce x]

comp :: CExpr a -> Expr b
comp c =
  let es = execWriter c
  in unsafeCoerce $ foldr (\(Any f) e -> Seq f e) Nop es

cexpr :: CExpr a -> LMan ()
cexpr = expr . comp

while :: Expr Int -> Expr a -> CExpr ()
while cond body = e $ While cond body

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

(./=) :: Expr Int -> Expr Int -> Expr Int
a ./= b = 1 - (a .== b)

(.<) :: Expr Int -> Expr Int -> Expr Int
(.<) = flip Cgt

(.<=) :: Expr Int -> Expr Int -> Expr Int
(.<=) = flip Cgte

(.>) :: Expr Int -> Expr Int -> Expr Int
(.>) = Cgt

(.>=) :: Expr Int -> Expr Int -> Expr Int
(.>=) = Cgte

atom :: Expr a -> Expr Int
atom = Atom

isNull :: Expr a -> Expr Int
isNull = Atom

tellc :: String -> LMan ()
tellc s = tell [const s]

compileExpr :: Expr a -> LMan ()
compileExpr e = case e of
  Nop -> return ()

  Const n -> ldc n

  Var i j -> ld i j

  Bin opr a b -> do
    compileExpr a
    compileExpr b
    tellc $ show opr

  Cons a b -> do
    compileExpr a
    compileExpr b
    tellc "CONS"

  Car a -> do
    compileExpr a
    tellc "CAR"

  Cdr a -> do
    compileExpr a
    tellc "CDR"

  Ceq a b -> do
    compileExpr a
    compileExpr b
    tellc "CEQ"

  Cgt a b -> do
    compileExpr a
    compileExpr b
    tellc "CGT"

  Cgte a b -> do
    compileExpr a
    compileExpr b
    tellc "CGTE"

  Atom e -> do
    compileExpr e
    tellc "ATOM"

  Dbug e -> do
    compileExpr e
    tellc "DBUG"

  Seq a b -> do
    compileExpr a
    compileExpr b

  While cond body -> do
    beg <- newLabel
    bod <- newLabel
    end <- newLabel

    emitLabel beg
    compileExpr cond
    tsel bod end

    emitLabel bod
    compileExpr body
    jmp beg

    emitLabel end

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
    tellc $ "ST  " ++ show (lev - i) ++ " " ++ show j

  With v f -> do
    l <- newLabel
    end <- newLabel
    compileExpr v
    ldf l
    tellc "AP 1"
    jmp end

    emitLabel l
    block $ do
      v <- innerVar 0
      compileExpr $ f v
      tellc "RTN"

    emitLabel end

  Closure name ->
    ldclo name

  Call1 name v1 -> do
    compileExpr v1
    ldclo name
    tellc "AP 1"

  Call2 name v1 v2 -> do
    compileExpr v1
    compileExpr v2
    ldclo name
    tellc "AP 2"

  Call3 name v1 v2 v3 -> do
    compileExpr v1
    compileExpr v2
    compileExpr v3
    ldclo name
    tellc "AP 3"

  Call4 name v1 v2 v3 v4 -> do
    compileExpr v1
    compileExpr v2
    compileExpr v3
    compileExpr v4
    ldclo name
    tellc "AP 4"


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
  tellc $ "LD " ++ show (lev - i) ++ " " ++ show j

ldclo :: String -> LMan ()
ldclo name = do
  lev <- gets csEnvLevel
  tell [ \tbl ->
          let ix = fromMaybe (error $ "undefined function: " ++ name) $ lookup name $ zip tbl [0..]
          in "LD " ++ show lev ++  " " ++ show ix
       ]

ldc :: Int -> LMan ()
ldc n = tellc $ "LDC " ++ show n

ldf :: Label -> LMan ()
ldf (Label l) = tellc $ "LDF " ++ l

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
  tellc $ "TSEL " ++ t ++ " " ++ e

jmp :: Label -> LMan ()
jmp l = do
  tellc "LDC 0"
  tsel l l

emitLabel :: Label -> LMan ()
emitLabel (Label l) = do
  tellc $ l ++ ":"

ite :: Expr Int -> Expr a -> Expr a -> Expr a
ite = Ite

(~=) :: Expr a -> Expr a -> CExpr () -- Expr ()
(Var i j) ~= v = e $ Assign i j v
_ ~= _ = error $ "Left hand side of := must be variable"

codeGen :: LMan () -> [String]
codeGen p =
  let (st, codes) = execRWS p () initState

      funcs = csFuncs st

      hdr = do
        tellc $ "DUM " ++ show (length funcs)
        forM_ funcs $ \(_name, Label l) -> do
          tellc $ "LDF " ++ l

        tellc "LDF main"
        tellc $ "TRAP " ++ show (length funcs)
        emitLabel $ Label "main"

      (_, header) = execRWS hdr () st
  in map (\f -> f $ map fst $ csFuncs st) $ header ++ codes

-----

with :: Expr a -> (Expr a -> Expr r)  -> Expr r
with = With

-----

footer :: LMan ()
footer = tellc "RTN"

header :: LMan ()
header = do
  undefined

compile :: LMan () -> String
compile = unlines . desugar . compile'

compile' :: LMan () -> [String]
compile' = map f.  codeGen . (>> footer) where
  f s
    | last s == ':' = s
    | otherwise = "  " ++ s

-----

expr :: Expr a -> LMan ()
expr e = compileExpr e

{-
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
  return $ traceShow ("fun", lev) $ Closure lev 0
-}

-----

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

-----

-- Library

def1 :: String -> (Expr a1 -> Expr r) -> (Expr a1 -> Expr r, LMan ())
def1 fname f = (Call1 fname, go) where
  go = do
    fun <- newLabel
    end <- newLabel
    addFunc fname fun

    jmp end

    emitLabel fun
    local $ do
      a1 <- innerVar 0
      compileExpr $ f a1
      tellc "RTN"

    emitLabel end
    return ()

def2 :: String -> (Expr a1 -> Expr a2 -> Expr r) -> (Expr a1 -> Expr a2 -> Expr r, LMan ())
def2 fname f = (Call2 fname, go) where
  go = do
    fun <- newLabel
    end <- newLabel
    addFunc fname fun

    jmp end

    emitLabel fun
    local $ do
      a1 <- innerVar 0
      a2 <- innerVar 1
      compileExpr $ f a1 a2
      tellc "RTN"

    emitLabel end
    return ()

def3 :: String -> (Expr a1 -> Expr a2 -> Expr a3 -> Expr r) -> (Expr a1 -> Expr a2 -> Expr a3 -> Expr r, LMan ())
def3 fname f = (Call3 fname, go) where
  go = do
    fun <- newLabel
    end <- newLabel
    addFunc fname fun

    jmp end

    emitLabel fun
    local $ do
      a1 <- innerVar 0
      a2 <- innerVar 1
      a3 <- innerVar 2
      compileExpr $ f a1 a2 a3
      tellc "RTN"

    emitLabel end
    return ()

def4:: String -> (Expr a1 -> Expr a2 -> Expr a3 -> Expr a4 -> Expr r) -> (Expr a1 -> Expr a2 -> Expr a3 -> Expr a4 -> Expr r, LMan ())
def4 fname f = (Call4 fname, go) where
  go = do
    fun <- newLabel
    end <- newLabel
    addFunc fname fun

    jmp end

    emitLabel fun
    local $ do
      a1 <- innerVar 0
      a2 <- innerVar 1
      a3 <- innerVar 2
      a4 <- innerVar 3
      compileExpr $ f a1 a2 a3 a4
      tellc "RTN"

    emitLabel end
    return ()

(&&&) :: Expr Int -> Expr Int -> Expr Int
a &&& b = a * b

(|||) :: Expr Int -> Expr Int -> Expr Int
a ||| b = lnot $ lnot a &&& lnot b

lnot :: Expr Int -> Expr Int
lnot e = 1 - e

debug = e. dbug
debugn = e.dbugn

i :: Expr Int -> Expr Int
i = id

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

libDef :: LMan ()
libDef = do
  nthDef
  updDef
  getMatDef
  setMatDef
