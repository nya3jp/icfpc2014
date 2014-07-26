#!/bin/bash

cd "$(dirname "$0")"

set -e

g++ -std=c++11 ../src/ghost-compiler/main.cc -o ghostcompiler

for i in *.ghci; do
  echo $i
  ./ghostcompiler < $i > ${i%%i}
done
