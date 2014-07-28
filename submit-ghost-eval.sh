#!/bin/bash

if [[ -z "$1" ]]; then
  echo "usage: $0 ghost1.ghc ghost2.ghc ..."
  exit 1
fi

key=$(for i in "$@"; do
  basename $i
done | sed 's/\..*//' | tr '\n' ',' | sed 's/,$//')
tar cf /tmp/ghost.tar "$@"
curl --basic --user einclad:asunyan -F key="$key" -F code=@/tmp/ghost.tar http://einclad.coders.jp/ghost/submit

echo "submitted. check your submission at http://einclad.coders.jp/ghost/"
