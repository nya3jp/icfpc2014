#!/bin/bash

if [[ -z "$1" ]]; then
  echo "usage: $0 your-ai.gcc"
  exit 1
fi

for i in $@; do
  curl --basic --user einclad:asunyan -F user=$USER -F url= -F comment="$i" -F code=@"$i" http://einclad.coders.jp/submit
  echo "submitted $i"
done

echo "check your submission at http://einclad.coders.jp/"
