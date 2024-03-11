#!/bin/bash

hyperfine_runs=1 

here=$(dirname "$0")
dice_range="${1:-64}"
stop_prob="${2:-0.5}"
dice="dice"

hyperfine --export-json "$here/pcfg-dice-stop_prob_${stop_prob}.json" \
          --export-csv  "$here/pcfg-dice-stop_prob_${stop_prob}.csv"  \
          --runs "$hyperfine_runs" \
          --parameter-scan length 1 "$dice_range" \
          --setup "perl $here/pcfg.dice.pl {length} ${stop_prob} >/tmp/{length}.dice" \
          --cleanup "rm /tmp/{length}.dice" \
          --output inherit \
          "$dice /dev/stdin </tmp/{length}.dice"
