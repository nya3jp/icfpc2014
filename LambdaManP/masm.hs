#!/usr/bin/env runhaskell
import Control.Applicative
import qualified Data.Map as M
import Data.Char
import Data.Maybe

main :: IO ()
main = interact desugar

desugar :: String -> String
desugar s =
  let ls = filter (not . null . words) . map removeComment  . lines $ s
      (adr, codes) = collectAddress ls
      dest = map (subst adr) codes
  in unlines dest

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

isLabelChar c = isAlpha c || isDigit c || c == '_'

removeComment = takeWhile (/= ';')
