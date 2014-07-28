{-# LANGUAGE ViewPatterns #-}

import Control.Applicative
import Control.Monad.RWS
import Data.Maybe
import Data.Monoid
import Text.Trifecta
import Text.Parser.Token.Style
import System.Environment hiding (lookupEnv)

data SExpr
  = SInt Integer
  | SSymbol String
  | SNil
  | SCons SExpr SExpr
  deriving (Eq, Show)

ppSExpr :: SExpr -> String
ppSExpr sexpr = case sexpr of
  SInt n -> show n
  SSymbol s -> show s
  SNil -> "()"
  SCons car cdr -> "(" ++ ppSExpr car ++ " . " ++ ppSExpr cdr ++  ")"

symbolString (SSymbol s) = Just s
symbolString _ = Nothing

toList :: SExpr -> Maybe [SExpr]
toList (SCons hd tl) = (hd :) <$> toList tl
toList SNil = Just []
toList _ = Nothing

lispIdents =
  emptyIdents { _styleLetter = _styleLetter emptyIdents <|> oneOf "+-+*=/!<>?" }

parseSExpr :: Parser SExpr
parseSExpr =
  parseCons <|>
  parseQuote <|>
  (SSymbol <$> ident lispIdents) <|>
  (SInt <$> integer)

parseCons :: Parser SExpr
parseCons = parens $ do
  es <- some parseSExpr
  cdr <- fromMaybe SNil <$> optional (symbolic '.' >> parseSExpr)
  return $ foldr SCons cdr es

parseQuote :: Parser SExpr
parseQuote = do
  _ <- symbolic '\''
  e <- parseSExpr
  return $ SCons (SSymbol "quote") $ SCons e SNil

parse :: FilePath -> IO [SExpr]
parse ss = do
  res <- parseFromFileEx (many (spaces >> parseSExpr)) ss
  case res of
    Failure err -> error $ show err
    Success r -> return r

-----

data Syntax
  = SConst SExpr
  | SVar String
  | SCall Syntax [Syntax]
  | SLambda [String] Syntax
  | SDefine String Syntax
  | SBegin [Syntax]
  | SIf Syntax Syntax Syntax
  deriving (Eq, Show)

parseSyntax :: SExpr -> Syntax
parseSyntax sexpr = case sexpr of
  SInt n -> SConst (SInt n)
  SSymbol sym -> SVar sym

  (toList -> Just [SSymbol "quote", e]) -> SConst e

  (toList -> Just (SSymbol "lambda": (toList -> Just args): body))
    | all (isJust . symbolString) args ->
      SLambda (catMaybes $ map symbolString args) $ SBegin $ map parseSyntax body

  (toList -> Just (SSymbol "begin": body)) ->
    SBegin $ map parseSyntax body

  (toList -> Just [SSymbol "if", cond, t, e]) ->
    SIf (parseSyntax cond) (parseSyntax t) (parseSyntax e)

  (toList -> Just (SSymbol "define": (toList -> Just a@((symbolString -> Just fname):args)): body))
    | all (isJust . symbolString) a ->
      SDefine (fname) $
      SLambda (catMaybes $ map symbolString args) $
      SBegin $ map parseSyntax body

  (toList -> Just [SSymbol "define", (symbolString -> Just var), val]) ->
    SDefine var $ parseSyntax val

  (toList -> Just (func: args)) ->
    SCall (parseSyntax func) (map parseSyntax args)

  _ ->
    error $ "compile error: " ++ show (ppSExpr sexpr)

-----

data Label = Label String deriving (Eq, Show)

data GCC
  = LDC Integer
  | LD Int Int
  | ST Int Int
  | ADD | SUB | MUL | DIV | CEQ | CGT | CGTE
  | ATOM | CONS | CAR | CDR
  | SEL Label Label | JOIN
  | LDF Label
  | AP Int
  | RTN
  | DUM
  | RAP
  | TSEL Label Label
  | TAP Int
  | TRAP Int
  | DBUG
  | BRK
  | LLabel Label
  deriving (Eq, Show)

data CompilerState
  = CompilerState
    { csEnv :: [[String]]
    , csUniq :: Int
    }

initCompilerState :: CompilerState
initCompilerState =
  CompilerState [] 0

type Compiler = RWS () [GCC] CompilerState

withEnv :: [String] -> Compiler a -> Compiler a
withEnv frame m = do
  org <- get
  when (not $ null frame) $ do
    modify $ \s -> s { csEnv = frame : csEnv s }
  r <- m
  put org
  return r

lookupEnv :: String -> Compiler (Int, Int)
lookupEnv name = do
  env <- gets csEnv
  case [ (i, j) | (i, frame) <- zip [0..] env, (j, var) <- zip [0..] frame, var == name ] of
    (r: _) -> return r
    _ -> error $ "undefined variable: " ++ show name

-- SPECIAL VARS
dustbox, world, ghosts :: String
dustbox = "*DUSTBOX*"
world   = "*WORLD*"
ghosts  = "*GHOSTS*"

newLabel :: Compiler Label
newLabel = do
  s <- get
  put $ s { csUniq = csUniq s + 1 }
  return $ Label $ "L" ++ show (csUniq s)

compileToGCC :: [Syntax] -> [GCC]
compileToGCC ss = snd $ evalRWS stub () initCompilerState where
  stub = do
    withEnv [world, ghosts] $ do
      compileSS $
        [SDefine dustbox (SConst (SInt 0))] ++
        ss ++
        [SCall (SVar "main") [SVar world, SVar ghosts]]

compileSS :: [Syntax] -> Compiler ()
compileSS ss = do
  let defs = [ varName | SDefine varName _ <- ss ]

  withEnv defs $ do
    case ss of
      [] ->
        tell [LDC 0]

      _ -> do
        forM_ (init ss) $ \syn -> do
          compileSyntax syn
          pop
        compileSyntax $ last ss

uniOp :: GCC -> Syntax -> Compiler ()
uniOp op a = do
  compileSyntax a
  tell [op]

binOp :: GCC -> Syntax -> Syntax -> Compiler ()
binOp op a b = do
  compileSyntax a
  compileSyntax b
  tell [op]

compileSyntax :: Syntax -> Compiler ()
compileSyntax s = case s of
  -----

  SCall (SVar "+")  [a, b] -> binOp ADD  a b
  SCall (SVar "-")  [a, b] -> binOp SUB  a b
  SCall (SVar "*")  [a, b] -> binOp MUL  a b
  SCall (SVar "/")  [a, b] -> binOp DIV  a b

  SCall (SVar "=")  [a, b] -> binOp CEQ  a b
  SCall (SVar ">")  [a, b] -> binOp CGT  a b
  SCall (SVar ">=") [a, b] -> binOp CGTE a b
  SCall (SVar "<")  [a, b] -> binOp CGT  b a
  SCall (SVar "<=") [a, b] -> binOp CGTE b a

  SCall (SVar "atom") [a]    -> uniOp ATOM a
  SCall (SVar "cons") [a, b] -> binOp CONS b a
  SCall (SVar "car")  [a]    -> uniOp CAR a
  SCall (SVar "cdr")  [a]    -> uniOp CDR a

  SCall (SVar "dbug") [a] -> uniOp DBUG a

  -----

  SConst sexpr ->
    compileConst sexpr

  SVar var -> do
    (i, j) <- lookupEnv var
    tell [LD i j]

  SCall func args -> do
    mapM_ compileSyntax args
    compileSyntax func
    tell [AP $ length args]

  SLambda args body -> do
    [fun, end] <- replicateM 2 newLabel
    jmp end

    tell [LLabel fun]
    withEnv args $ do
      compileSyntax body
      tell [RTN]

    tell [LLabel end]
    tell [LDF fun]

  SBegin ss ->
    compileSS ss

  SIf cond t e -> do
    [lt, le, end] <- replicateM 3 newLabel

    compileSyntax cond
    tell [TSEL lt le]

    tell [LLabel lt]
    compileSyntax t
    jmp end

    tell [LLabel le]
    compileSyntax e

    tell [LLabel end]

  SDefine var e -> do
    compileSyntax e
    (i, j) <- lookupEnv var
    tell [ST i j]
    tell [LD i j] -- define returns passed value

jmp :: Label -> Compiler ()
jmp lbl = tell [LDC 0, TSEL lbl lbl]

pop :: Compiler ()
pop = do
  (i, j) <- lookupEnv dustbox
  tell [ST i j]

compileConst :: SExpr -> Compiler ()
compileConst e = case e of
  SInt n -> tell [LDC n]
  SNil -> tell [LDC 0]

  SCons car cdr -> do
    compileConst car
    compileConst cdr
    tell [CAR]

  SSymbol var -> do
    error $ "symbol value does not supported: " ++ var

outputGCCWithLabel :: [GCC] -> IO ()
outputGCCWithLabel ops = do
  forM_ ops $ \op -> case op of
    LLabel (Label l) -> putStrLn $ l ++ ":"

    SEL (Label l1) (Label l2) ->
      putStrLn $ "  SEL " ++ l1 ++ " " ++ l2

    TSEL (Label l1) (Label l2) ->
      putStrLn $ "  TSEL " ++ l1 ++ " " ++ l2

    LDF (Label l) ->
      putStrLn $ "  LDF " ++ l

    _ -> putStrLn $ "  " ++ show op

-----

main :: IO ()
main = do
  [file] <- getArgs
  sexpr <- parse file
  mapM_ (putStrLn . ppSExpr) sexpr

  let progn = map parseSyntax sexpr
  mapM_ print progn

  let gcc = compileToGCC progn
  outputGCCWithLabel gcc

  return ()
