#!/bin/bash

if [[ -z "$1" ]]; then
  echo "usage: $0 your-ai.gcc"
  exit 1
fi

curl --basic --user einclad:asunyan -F user=$USER -F url= -F comment="$1" -F code=@"$1" http://einclad.coders.jp/submit

echo "submitted. check your submission at http://einclad.coders.jp/"
