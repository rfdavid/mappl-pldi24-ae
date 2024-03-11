#!/bin/bash

hyperfine_runs=1 

here=$(dirname "$0")
cd $here
wd=$(pwd)
echo "working dir:" $wd
EXEC="python -u hmm_mixed.mappl.py"

exponent_ub="${1:-20}"
horizon="${2:-1}"
alpha="${3:-1}"
beta="${4:-1}"
theta0="${5:-0.3}"
theta1="${6:-0.7}"
theta="${theta0} ${theta1}"

param_list="horizon_${horizon}-exponent_ub_${exponent_ub}-alpha_${alpha}-beta_${beta}-theta_${theta0}_${theta1}"
file_name="hmm_mixed.mappl.${param_list}"

echo ${param_list}

echo
echo "Running MAPPL-IS"
echo
{
hyperfine --runs "$hyperfine_runs" \
          --parameter-scan exponent 1 ${exponent_ub} \
          --style none \
          --output inherit \
          --ignore-failure \
          "$EXEC --config MAPPL-IS --num_samples_exponent {exponent} --alpha ${alpha}  --beta ${beta}  --init 1 --horizon ${horizon} --seed 0 --theta ${theta}"
        #   --export-json "$here/${file_name}.json" \
        #   --export-csv  "$here/${file_name}.csv"  \
} 2>&1 | tee  ${here}/${file_name}.log.csv
