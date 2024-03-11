#!/bin/bash

hyperfine_runs=1 

here=$(dirname "$0")
cd $here
wd=$(pwd)
echo "working dir:" $wd
EXEC="python -u hmm_mixed.mapplexact.py"

horizon="${1:-1}"
alpha="${2:-1}"
beta="${3:-1}"
theta0="${4:-0.3}"
theta1="${5:-0.7}"
theta="${theta0} ${theta1}"

param_list="horizon_${horizon}-alpha_${alpha}-beta_${beta}-theta_${theta0}_${theta1}"
file_name="hmm_mixed.exact.${param_list}"

echo ${param_list}

echo
echo "Running MAPPL-EXACT"
echo
{
hyperfine --runs "$hyperfine_runs" \
          --style none \
          --output inherit \
          --ignore-failure \
          "$EXEC --config MAPPL-SI --alpha ${alpha}  --beta ${beta}  --init 1 --horizon ${horizon} --seed 0 --theta ${theta}"
        #   --export-json "$here/${file_name}.json" \
        #   --export-csv  "$here/${file_name}.csv"  \
} 2>&1 | tee  ${here}/${file_name}.log.csv
