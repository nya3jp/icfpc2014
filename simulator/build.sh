#!/bin/sh

set -eu

ocamlbuild -tag use_str main.native
mv main.native simulator
ocamlbuild lambdaman_run.native
mv lambdaman_run.native lambdaman_run
ocamlbuild ghost_run.native
mv ghost_run.native ghost_run
ocamlbuild ghost_steprun.native
mv ghost_steprun.native ghost_steprun
ocamlbuild unittest.native
mv unittest.native unittest

