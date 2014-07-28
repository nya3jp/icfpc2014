{-# LANGUAGE FlexibleInstances, GADTs, RecursiveDo, ScopedTypeVariables, Rank2Types, LiberalTypeSynonyms, ImpredicativeTypes, NoMonoLocalBinds #-}

module DSL where

import Control.Monad.RWS hiding (local, Any)
import Control.Monad.State
import Control.Monad.Writer hiding (Any)
import Data.Maybe
import Debug.Trace
import Unsafe.Coerce

import Desugar
import Optimize

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

conf_xhl_optimizer = False

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


  Ite :: Expr Int -> Expr a -> Expr a -> Expr a
  Assign :: Int -> Int -> Expr a -> Expr ()

  Seq :: Expr a -> Expr b -> Expr b
  While :: Expr Int -> Expr a -> Expr ()
  With :: [Any] -> ([Any] -> Expr r) -> Expr r

  CallG :: String -> [Any] -> Expr r
  Closure :: String -> Expr a

data Any = Any (forall a. Expr a)

unAny :: Any -> a
unAny (Any v) = unsafeCoerce v

type CExpr a  = Writer [Any]

comp :: CExpr a () -> Expr a
comp c =
  let es = execWriter c
  in if null es
     then unsafeCoerce $ foldr (\(Any f) e -> Seq f e) Nop es
     else unsafeCoerce $ foldr (\(Any f) e -> Seq f e) (unAny $ last es) $ init es

emitComp :: CExpr a () -> LMan ()
emitComp = emitExpr . comp

while :: Expr Int -> CExpr a () -> CExpr a ()
while cond body = e $ While cond (comp body)

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

infix 4 .==, ./=, .<, .<=, .>, .>=

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

nooptCondExpr0 :: Expr Int -> ([Expr Int], [Expr Int])
nooptCondExpr0 ee = ([ee], [Ceq ee (Const 0)])

optCondExpr0 :: Expr Int -> ([Expr Int], [Expr Int])
optCondExpr0 ee = case (optExpr (optExpr ee)) of
  Const n -> ([Const n], [Const (if n == 0 then 1 else 0)])
  Bin SUB (Const 1) (Ceq a b) -> ([ee], [Ceq a b])
  Bin SUB (Ceq a b) (Const 1) -> ([ee], [Ceq a b])
  Ite c a b -> 
    let (aa,not_aa) = optCondExpr0 a in
    let (bb,not_bb) = optCondExpr0 b in
    let (cc,not_cc) = optCondExpr0 c in
    ([Ite ccc     aaa bbb | ccc     <- cc,     aaa <- aa, bbb <- bb] ++
     [Ite not_ccc bbb aaa | not_ccc <- not_cc, aaa <- aa, bbb <- bb],
     [Ite ccc     not_aaa not_bbb | ccc     <- cc,     not_aaa <- not_aa, not_bbb <- not_bb] ++
     [Ite not_ccc not_bbb not_aaa | not_ccc <- not_cc, not_aaa <- not_aa, not_bbb <- not_bb])
  Seq a b -> let (bb,not_bb) = optCondExpr0 b in ([Seq a bbb | bbb <- bb], [Seq a not_bbb | not_bbb <- not_bb])
  _ -> nooptCondExpr0 ee

optCondExpr :: Expr Int -> (Bool, Expr Int)
optCondExpr cond = if not conf_xhl_optimizer then (False, cond) else
  let (conds,not_conds) = optCondExpr0 cond in
  let (cond_best_len,    cond_best)     = minimum $ zip (map (length . codeGen . compileExpr) conds) conds in
  let (not_cond_best_len,not_cond_best) = minimum $ zip (map (length . codeGen . compileExpr) not_conds) not_conds in
  if not_cond_best_len < cond_best_len then
    (True, not_cond_best)
  else
    (False, cond_best)

optExpr :: Expr a -> Expr a  -- ()
optExpr ee = if not conf_xhl_optimizer then ee else case ee of
  Car (Cons a b) -> optExpr a
  Cdr (Cons a b) -> optExpr b
  Bin op a b -> Bin op (optExpr a) (optExpr b)
  Cons a b -> Cons (optExpr a) (optExpr b)
  Car a -> Car (optExpr a)
  Cdr a -> Cdr (optExpr a)
  Ceq a b -> Ceq (optExpr a) (optExpr b)
  Cgt a b -> Cgt (optExpr a) (optExpr b)
  Cgte a b -> Cgte (optExpr a) (optExpr b)
  Atom a -> Atom (optExpr a)
  Assign i j a -> Assign i j (optExpr a)
  Seq a b -> Seq (optExpr a) (optExpr b)
  Ite cond aa bb ->
    let (reversed,cond2) = optCondExpr (optExpr cond) in
    if reversed then
      Ite cond2 (optExpr bb) (optExpr aa)
    else
      Ite cond2 (optExpr aa) (optExpr bb)
{-
  While cond body ->
    let (reversed,cond2) = optCondExpr (optExpr cond) in
    While2 reversed cond2 (optExpr body)
  While2 reversed0 cond body ->
    let (reversed,cond2) = optCondExpr (optExpr cond) in
    While2 (reversed0 /= reversed) cond2 (optExpr body)
-}
  _ -> ee


compileTselExpr :: Bool -> Expr Int -> Label -> Label -> LMan ()
compileTselExpr reversed0 cond tlabel elabel = do
  let (reversed,cond2) = optCondExpr cond
  case reversed0 /= reversed of
    True -> do
      compileExpr cond2
      tsel elabel tlabel
    False -> do
      compileExpr cond2
      tsel tlabel elabel

compileExpr :: Expr a -> LMan ()
compileExpr ee = case optExpr ee of
  Nop -> ldc 0

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
    ldc 0

  Seq a b -> do
    compileExpr a
    pop
    compileExpr b

  Ite cond t e -> do
    tlabel <- newLabel
    elabel <- newLabel
    jlabel <- newLabel
    compileTselExpr False cond tlabel elabel
    emitLabel tlabel
    compileExpr t
    jmp jlabel
    emitLabel elabel
    compileExpr e
    emitLabel jlabel

  Closure name ->
    ldclo name

  CallG name vs -> do
    mapM_ (\(Any a) -> compileExpr a) vs
    ldclo name
    tellc $ "AP " ++ show (length vs)

  Assign i j e -> do
    compileExpr e
    st i j
    ldc 0

  While cond body -> do
    beg <- newLabel
    bod <- newLabel
    end <- newLabel

    emitLabel beg
    compileTselExpr False cond bod end

    emitLabel bod
    compileExpr body
    pop
    jmp beg

    emitLabel end
    ldc 0

  With vs f -> do
    l <- newLabel
    end <- newLabel
    mapM_ (\(Any v) -> compileExpr v) vs
    ldf l
    tellc $ "AP " ++ show (length vs)
    jmp end

    emitLabel l
    local $ do
      as <- mapM innerVar [0..length vs - 1]
      compileExpr $ f $ map (Any . unsafeCoerce) as
      tellc "RTN"

    emitLabel end

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

st :: Int -> Int -> LMan ()
st i j = do
  lev <- gets csEnvLevel
  tellc $ "ST " ++ show (lev - i) ++ " " ++ show j

pop :: LMan ()
pop = do
  lev <- gets csEnvLevel
  tellc $ "ST " ++ show (lev - 0) ++ " 0 ; POP"

ldclo :: String -> LMan ()
ldclo name = do
  lev <- gets csEnvLevel
  tell [ \tbl ->
          let ix = fromMaybe (error $ "undefined function: " ++ name) $ lookup name $ zip tbl [1 ..]
          in "LD " ++ show lev ++  " " ++ show ix
       ]

ldc :: Int -> LMan ()
ldc n = tellc $ "LDC " ++ show n

ldf :: Label -> LMan ()
ldf (Label l) = tellc $ "LDF " ++ l

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

-----

codeGen :: LMan () -> [String]
codeGen p =
  let (st, codes) = execRWS p () initState

      funcs = csFuncs st

      hdr = do
        tellc $ "DUM " ++ show (length funcs + 1)
        ldc 0
        forM_ funcs $ \(_name, Label l) -> do
          tellc $ "LDF " ++ l

        tellc "LDF main"
        tellc $ "TRAP " ++ show (length funcs + 1)
        emitLabel $ Label "main"

      (_, header) = execRWS hdr () st
  in map (\f -> f $ map fst $ csFuncs st) $ header ++ codes

-----

footer :: LMan ()
footer = tellc "RTN"

compile :: LMan () -> String
compile = unlines . desugar . compile'

compile' :: LMan () -> [String]
compile' = optimize . map f.  codeGen . (>> footer) where
  f s
    | last s == ':' = s
    | otherwise = "  " ++ s

-----

def' fname call = do
  -- fun <- newLabel
  let fun = Label fname
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

with :: Expr a -> (Expr a -> CExpr r ()) -> CExpr r ()
with a1 f = e $ With [Any $ cast a1] $ comp . (\[a1] -> f (unAny a1))

with2 :: Expr a1 -> Expr a2 -> (Expr a1 -> Expr a2 -> CExpr r ()) -> CExpr r ()
with2 a1 a2 f = e $ With [Any $ cast a1, Any $ cast a2] $ comp . (\[a1, a2] -> f (unAny a1) (unAny a2))

with3 :: Expr a1 -> Expr a2 -> Expr a3 -> (Expr a1 -> Expr a2 -> Expr a3 -> CExpr r ()) -> CExpr r ()
with3 a1 a2 a3 f = e $ With [Any $ cast a1, Any $ cast a2, Any $ cast a3] $ comp . (\[a1, a2, a3] -> f (unAny a1) (unAny a2) (unAny a3))

with4 :: Expr a1 -> Expr a2 -> Expr a3 -> Expr a4 -> (Expr a1 -> Expr a2 -> Expr a3 -> Expr a4 -> CExpr r ()) -> CExpr r ()
with4 a1 a2 a3 a4 f = e $ With [Any $ cast a1, Any $ cast a2, Any $ cast a3, Any $ cast a4] $ comp . (\[a1, a2, a3, a4] -> f (unAny a1) (unAny a2) (unAny a3) (unAny a4))

with5 :: Expr a1 -> Expr a2 -> Expr a3 -> Expr a4 -> Expr a5 -> (Expr a1 -> Expr a2 -> Expr a3 -> Expr a4 -> Expr a5 -> CExpr r ()) -> CExpr r ()
with5 a1 a2 a3 a4 a5 f = e $ With [Any $ cast a1, Any $ cast a2, Any $ cast a3, Any $ cast a4, Any $ cast a5] $ comp . (\[a1, a2, a3, a4, a5] -> f (unAny a1) (unAny a2) (unAny a3) (unAny a4) (unAny a5))

-----

dbug :: Expr a -> Expr ()
dbug e = Dbug e

dbugn :: Expr Int -> Expr ()
dbugn = dbug

debug :: Expr a -> CExpr () ()
debug = e . dbug

debugn :: Expr Int -> CExpr () ()
debugn = e . dbugn

cons :: Expr a -> Expr b -> Expr (a, b)
cons = Cons

c :: Expr Int -> Expr Int
c = id

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

ite :: Expr Int -> Expr a -> Expr a -> Expr a
ite = Ite

cond :: Expr Int -> CExpr a () -> CExpr a () -> CExpr a ()
cond c a b = e $ ite c (comp a) (comp b)

lwhen :: Expr Int -> CExpr a () -> CExpr a ()
lwhen c a = e $ ite c (comp a) (Const 0)


infix 5 ~=
(~=) :: Expr a -> Expr a -> CExpr () ()
(Var i j) ~= v = e $ Assign i j v
_ ~= _ = error $ "Left hand side of := must be variable"

emitExpr :: Expr a -> LMan ()
emitExpr e = compileExpr e >> pop

rtn :: Expr a -> LMan ()
rtn e = compileExpr e

e :: Expr a -> CExpr a ()
e x = tell [Any $ unsafeCoerce x]

gcons :: Expr a -> Expr b -> Expr c
gcons = unsafeCoerce cons

gcar :: Expr a -> Expr b
gcar = unsafeCoerce car

gcdr :: Expr a -> Expr b
gcdr = unsafeCoerce cdr

cast :: Expr a -> Expr b
cast = unsafeCoerce

for :: Expr Int -> Expr Int -> (Expr Int -> CExpr a ()) -> CExpr a ()
for f t body =
  with f $ \i -> do
    while (i .< t) $ do
      body i
      i ~= i + 1

class Debuggable a where
  trace :: a -> CExpr () ()

instance Debuggable (Expr a) where
  trace = debug

instance Debuggable (Expr a1, Expr a2) where
  trace (v1, v2) = debug $ cons v1 v2

instance Debuggable (Expr a1, Expr a2, Expr a3) where
  trace (v1, v2, v3) = debug $ cons v1 $ cons v2 v3

instance Debuggable (Expr a1, Expr a2, Expr a3, Expr a4) where
  trace (v1, v2, v3, v4) = debug $ cons v1 $ cons v2 $ cons v3 v4

instance Debuggable (Expr a1, Expr a2, Expr a3, Expr a4, Expr a5) where
  trace (v1, v2, v3, v4, v5) = debug $ cons v1 $ cons v2 $ cons v3 $ cons v4 v5

instance Debuggable (Expr a1, Expr a2, Expr a3, Expr a4, Expr a5, Expr a6) where
  trace (v1, v2, v3, v4, v5, v6) = debug $ cons v1 $ cons v2 $ cons v3 $ cons v4 $ cons v5 v6
