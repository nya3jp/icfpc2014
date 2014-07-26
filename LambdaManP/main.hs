{-# LANGUAGE FlexibleInstances, GADTs, RecursiveDo #-}

import Control.Monad.RWS hiding (local)
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe
import Data.Char
import Debug.Trace
import System.Environment

-----

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

  Ite :: Expr Int -> Expr a -> Expr a -> Expr a
  With :: Expr a -> (Expr a -> Expr r) -> Expr r

  Seq :: Expr a -> Expr b -> Expr b

  Call1 :: Fun (a1 -> r) -> Expr a1 -> Expr r

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

  Call1 (Fun i j) v -> do
    compileExpr v
    ld i j
    tell ["AP 1"]

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

{-
fun0 :: LMan () -> LMan Fun
fun0 code = do
  flabel <- newLabel
  end <- newLabel
  jmp end

  emitLabel flabel
  s <- get
  put $ s { csEnvLevel = csEnvLevel s + 1 }
  code
  tell "RTN"
  put s

  emitLabel end

  -- return $ Fun
  undefined
-}

codeGen :: LMan () -> [String]
codeGen p =
  let ((), codes) = evalRWS p () initState
  in codes

-----

data Fun t = Fun Int Int

-- call0 :: Fun r -> Expr r
-- call0 (Fun i) = undefined

{-
call1 :: Fun (a1 -> r) -> Expr a1 -> Expr r
call1 (Fun i) e = do
  compileExpr e
  ld 0 i
  tell ["AP"]
-}

with :: Expr a -> (Expr a -> Expr r)  -> Expr r
with = With

-----

compile :: LMan () -> String
compile = unlines . desugar . codeGen

compile' :: LMan () -> String
compile' = unlines . map f.  codeGen where
  f s
    | last s == ':' = s
    | otherwise = "  " ++ s

-----

-- library


-----

{-
fun1 :: (Expr a1 -> LMan r) -> LMan r
fun1 f = do
  s <- get
  f (ld (csEnvLevel s + 1) 0)

fact :: Func (Expr Int -> Expr Int)
fact = fun $ \i -> do
  ite (i .== 0)
    (0)
    (i * call1 fact (i - 1))
-}

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

  -}

  -- dbug $ call hoge 1 2 (cons 1 2)
  undefined

fun1 :: (Expr a1 -> Expr r) -> LMan (Fun (a1 -> r))
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
  return $ Fun lev 0

call1 :: Fun (a1 -> r) -> Expr a1 -> Expr r
call1 = Call1

progn :: LMan ()
progn = do
  rec
    fact <- fun1 $ \i ->
      ite (i .== 0) 1 (i * call1 fact (i - 1))

  -- foo <- fun1 $ \i -> i + i * i

  expr $ dbugn $ call1 fact 10

  return ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["debug"] -> do
      putStrLn $ compile' progn
    _ -> do
      putStrLn $ compile progn
