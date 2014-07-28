#!/bin/bash

cd "$(dirname "$0")/.."

set -ex

rm -rf .build
mkdir -p .build
cp ./LambdaManP/ataittara-saikyo.gcc solution/lambdaman.gcc
