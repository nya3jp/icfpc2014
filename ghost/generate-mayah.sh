#!/bin/sh

for r in 8 16 24 32 48 64; do
  for c in 0 8 16 24 32; do
    for j in 0 8 16 24 32; do
      for b in 0 8 16 24 32; do
        sed -e "s/HOGE_RANDOM/$r/g" -e "s/HOGE_CHASE/$c/g" -e "s/HOGE_JUNCTION/$j/g" -e "s/HOGE_BODDY/$b/g" mayah.ghci > template.ghci
        echo $r $c $j $b
        ./ghostcompiler < template.ghci > mayah-$r-$c-$j-$b.ghc
      done
    done
  done
done