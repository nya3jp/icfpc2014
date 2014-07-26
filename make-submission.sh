#!/bin/bash

cd "$(dirname "$0")"

mkdir -p submissions
rm -rf .build
git archive --format=tar --prefix=.build/code/ HEAD | tar x
mkdir -p .build/solution
ln -s ../code/state-of-the-art.gcc .build/solution/lambdaman.gcc
cd .build
rm -rf submissions
zip -r ../submissions/.new.zip . > /dev/null 2>&1
cd ..
rm -rf .build
cd submissions

name="submission-$(date +%Y%m%d%H%M%S)-$(sha1sum .new.zip | sed 's/ .*//').zip"
mv .new.zip $name

unzip -l $name
if unzip -l $name solution/lambdaman.gcc > /dev/null 2>&1; then
  echo "Archive verified. Written to submissions/$name"
else
  echo "Archive verification failed. Please inspect."
  rm -f $name
fi
