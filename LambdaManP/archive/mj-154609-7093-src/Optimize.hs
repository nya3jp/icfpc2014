module Optimize (optimize) where

import qualified Data.Map.Strict as M

optimize :: [String] -> [String]
optimize = peephole . shortcutJmp . peephole

peephole :: [String] -> [String]
peephole [] = []
peephole ss = case ss of
  (l1: l2: rest)
    | isLoad l1 && isPop l2 -> peephole rest
    | getOpc l1 == "AP" && getOpc l2 == "RTN" ->
      ("TAP " ++ (words l1 !! 1)) : peephole rest
  (l1: l2: l3: rest)
    | isPop l1 && isJmp l2 l3 ->
      l3: peephole rest
  (l: rest) ->
    l: peephole rest

getOpc = head . words

isLoad s =
  let opc = getOpc s
  in opc == "LD" || opc == "LDC" || opc == "LDF"

isLDC s = getOpc s == "LDC"
isRTN s = getOpc s == "RTN"

isPop s = last (words s) == "POP"

shortcutJmp :: [String] -> [String]
shortcutJmp ss = subst ss where
  tbl :: M.Map String String
  tbl = M.fromList $ genTbl ss

  genTbl [] = []
  genTbl (l1: l2: l3: rest)
    | isLabel l1 && isJmp l2 l3 =
      (getLabel l1, words l3 !! 1): genTbl rest
  genTbl (l1: l2: rest)
    | isLabel l1 && isRTN l2 =
      (getLabel l1, ""): genTbl rest
  genTbl (_: rest) =
    genTbl rest

  dfs l = case M.lookup l tbl of
    Nothing -> l
    Just "" -> ""
    Just r -> dfs r

  jmpTo "" = ["RTN"]
  jmpTo l = ["LDC 0", "TSEL " ++ l ++ " " ++ l]

  subst [] = []
  subst (l1: l2: rest)
    | isJmp l1 l2 =
      jmpTo (dfs $ words l2 !! 1) ++ subst rest
  subst (l: rest) =
    l : subst rest

isLabel l = case (reverse $ unwords $ words l) of
  ':': _ -> True
  _ -> False

getLabel = init . unwords . words

isJmp l1 l2 = isLDC l1 && f l2 where
  f s = case words s of
    ["TSEL", t, e] | t == e -> True
    _ -> False

deadCodeElim :: [String] -> [String]
deadCodeElim = undefined
