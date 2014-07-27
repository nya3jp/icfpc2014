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
  if isJust (lookup name $ csFuncs s)
    then error $ "multiple function definition: " ++ name
    else put $ s { csFuncs = (name, l) : csFuncs s }

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

  CallG :: String -> [Any] -> Expr r
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

  CallG name vs -> do
    mapM_ (\(Any a) -> compileExpr a) vs
    ldclo name
    tellc $ "AP " ++ show (length vs)

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

expr :: Expr a -> LMan ()
expr e = compileExpr e

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

def' fname call = do
  fun <- newLabel
  end <- newLabel
  addFunc fname fun

  jmp end

  emitLabel fun
  local $ do
    call
    tellc "RTN"

  emitLabel end
  return ()

def1 :: String -> (Expr a1 -> Expr r) -> (Expr a1 -> Expr r, LMan ())
def1 fname f = (\v1 -> CallG fname [Any $ unsafeCoerce v1], def' fname (do a1 <- innerVar 0;compileExpr $ f a1))

def2 :: String -> (Expr a1 -> Expr a2 -> Expr r) -> (Expr a1 -> Expr a2 -> Expr r, LMan ())
def2 fname f = (\v1 v2 -> CallG fname [Any $ unsafeCoerce v1, Any $ unsafeCoerce v2], def' fname (do a1 <- innerVar 0; a2 <- innerVar 1; compileExpr $ f a1 a2))

def3 :: String -> (Expr a1 -> Expr a2 -> Expr a3 -> Expr r) -> (Expr a1 -> Expr a2 -> Expr a3 -> Expr r, LMan ())
def3 fname f = (\v1 v2 v3 -> CallG fname [Any $ unsafeCoerce v1, Any $ unsafeCoerce v2, Any $ unsafeCoerce v3], def' fname (do a1 <- innerVar 0; a2 <- innerVar 1; a3 <- innerVar 2; compileExpr $ f a1 a2 a3))

def4 :: String -> (Expr a1 -> Expr a2 -> Expr a3 -> Expr a4 -> Expr r) -> (Expr a1 -> Expr a2 -> Expr a3 -> Expr a4 -> Expr r, LMan ())
def4 fname f = (\v1 v2 v3 v4 -> CallG fname [Any $ unsafeCoerce v1, Any $ unsafeCoerce v2, Any $ unsafeCoerce v3, Any $ unsafeCoerce v4], def' fname (do a1 <- innerVar 0; a2 <- innerVar 1; a3 <- innerVar 2; a4 <- innerVar 3; compileExpr $ f a1 a2 a3 a4))

def5 :: String -> (Expr a1 -> Expr a2 -> Expr a3 -> Expr a4 -> Expr a5 -> Expr r) -> (Expr a1 -> Expr a2 -> Expr a3 -> Expr a4 -> Expr a5 -> Expr r, LMan ())
def5 fname f = (\v1 v2 v3 v4 v5 -> CallG fname [Any $ unsafeCoerce v1, Any $ unsafeCoerce v2, Any $ unsafeCoerce v3, Any $ unsafeCoerce v4, Any $ unsafeCoerce v5], def' fname (do a1 <- innerVar 0; a2 <- innerVar 1; a3 <- innerVar 2; a4 <- innerVar 3; a5 <- innerVar 4; compileExpr $ f a1 a2 a3 a4 a5))
