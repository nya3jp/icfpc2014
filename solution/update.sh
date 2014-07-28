#!/bin/bash

cd "$(dirname "$0")/.."

set -ex

rm -rf .build
mkdir -p .build
cp LambdaManP/madoka.asm .build/madoka.asm
python alice/alice.py -r alice/karen.py > .build/karen.asm
python alice/shino.py .build/madoka.asm .build/karen.asm > solution/lambdaman.gcc
