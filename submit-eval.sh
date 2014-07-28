#!/bin/bash

if [[ -z "$1" ]]; then
  echo "usage: $0 your-ai.gcc"
  exit 1
fi

i="$(basename $1 | sed 's/\..*//')"
curl --basic --user einclad:asunyan -F key="$i" -F code=@"$1" http://einclad.coders.jp/submit
echo "submitted $i"

echo "check your submission at http://einclad.coders.jp/"
