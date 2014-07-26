#!/usr/bin/env runhaskell
import System.Process 
import Text.Printf
main :: IO ()
main = do
  system "ls LambdaMan/gen/*.txt > tmp-filelist.txt"
  system "cat LambdaMan/gen/*.txt > tmp-content.txt"  
  system "paste tmp-content.txt  tmp-filelist.txt | sort -n > tmp-ranking.txt"
  str <- readFile "tmp-ranking.txt"
  let fn = last $ words $ str
      gccFn = (++".gcc")$reverse$drop 4$reverse$fn
  system $ printf "cp %s LambdaMan/bio-aznyan.gcc" gccFn
  return ()