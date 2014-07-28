#!/bin/bash

cd "$(dirname "$0")/.."

set -ex

rm -rf .build
mkdir -p .build
cp ./LambdaManP/saikyo.gcc solution/lambdaman.gcc
