#!/bin/bash

root="$(dirname "$0")/"

exec "$root/simulator/simulator" "-v" $1 $2 "$root/ghost/chase_with_random.ghc,$root/ghost/scatter.ghc,$root/ghost/random_and_chase.ghc"
