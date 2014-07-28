#!/bin/bash

root="$(dirname "$0")/../.."

exec "$root/simulator/simulator" "$@" "$root/map/world-classic.txt" /dev/stdin "$root/ghost/chase_with_random.ghc,$root/ghost/scatter.ghc,$root/ghost/random_and_chase.ghc"
