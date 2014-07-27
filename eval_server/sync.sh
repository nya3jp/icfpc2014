#!/bin/bash

rsync -av ~/src/icfpc2014/eval_server/. einclad:icfpc2014/eval_server/.
rsync -av ~/src/icfpc2014/eval_server/. eval:icfpc2014-eval/eval_server/.
