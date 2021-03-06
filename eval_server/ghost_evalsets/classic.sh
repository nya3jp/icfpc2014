#!/bin/bash

root="$(dirname "$0")/../.."

id="$(date +%s%N)"
tmpdir="$root/eval_server/tmp/$id"

mkdir -p "$tmpdir"
tar x -C "$tmpdir"

ghosts="$(find "$tmpdir" -type f | tr '\n' ',' | sed 's/,$//')"

exec "$root/simulator/simulator" "$root/map/world-classic.txt" "$root/LambdaMan/bio-aznyan.gcc" "$ghosts"
