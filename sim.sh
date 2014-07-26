#!/bin/bash
# Usage:
# ./sim.sh --map=map/sample.map --lambda=LambdaMan/bi-2.gcc --ghost=ghost/follow.ghc

NODE=
if [[ -z "$NODE" ]] && which node > /dev/null 2>&1; then
  NODE=node
fi
if [[ -z "$NODE" ]] && which nodejs > /dev/null 2>&1; then
  NODE=nodejs
fi

if [[ -z "$NODE" ]]; then
  echo "node.js is not installed!"
  exit 1
fi

if [[ $# = 0 ]]; then
  echo "Usage: sim.sh --map=map/sample.map --lambda=LambdaMan/bi-2.gcc --ghost=ghost/follow.ghc"
  exit 1
fi

exec $NODE "$(dirname "$0")/src/reference-simulator/simulator.js" "$@"
