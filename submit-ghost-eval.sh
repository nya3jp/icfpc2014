#!/bin/bash

if [[ -z "$1" ]]; then
  echo "usage: $0 ghost1.ghc ghost2.ghc ..."
  exit 1
fi

comment=$@
tar cf /tmp/ghost.tar "$@"
curl --basic --user einclad:asunyan -F user=$USER -F url= -F comment="$comment" -F code=@/tmp/ghost.tar http://localhost:8080/ghost/submit

echo "submitted. check your submission at http://einclad.coders.jp/ghost/"
