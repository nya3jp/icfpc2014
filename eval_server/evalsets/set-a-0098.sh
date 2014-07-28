#!/bin/bash

root="$(dirname "$0")/../.."

exec "$root/simulator/simulator" "$root/map/gen/eval0098.map" /dev/stdin "$root/ghost/chase_with_random.ghc,$root/ghost/scatter.ghc,$root/ghost/random_and_chase.ghc,$root/ghost/chase_fixed.ghc"
