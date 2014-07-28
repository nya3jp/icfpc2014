#!/bin/bash

if [[ -z "$1" ]]
then
  echo "usage: $0 your-ai.hs"
  echo "      your-ai.hs.gcci and your-ai.hs.gcc are generated"
  exit 1
fi

runhaskell $1 debug \
 | perl peephole.pl 2> /dev/null \
 | perl peephole.pl 2> /dev/null \
 | perl peephole.pl 2> /dev/null \
 | tee $1.gcci \
 | runhaskell desug.hs \
 > $1.gcc
