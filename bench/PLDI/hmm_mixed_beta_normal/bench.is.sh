#!/bin/bash

hyperfine_runs=1 

here=$(dirname "$0")
cd $here
wd=$(pwd)
echo "working dir:" $wd
EXEC="python -u hmm_mixed.is.py"

exponent_ub="${1:-20}"
horizon="${2:-1}"
alpha="${3:-1}"
beta="${4:-1}"
sigma="${5:-0.3}"
theta0="${6:-0.3}"
theta1="${7:-0.7}"
theta="${theta0} ${theta1}"

param_list="horizon_${horizon}-exponent_ub_${exponent_ub}-alpha_${alpha}-beta_${beta}-sigma_${sigma}-theta_${theta0}_${theta1}"
file_name="hmm_mixed.is.${param_list}"

echo ${param_list}

echo
echo "Running IS"
echo
{
$EXEC --config header --horizon 0 --alpha 0 --beta 0 --init 0 --horizon 0 --sigma 0  --theta 0 0
hyperfine --runs "$hyperfine_runs" \
          --parameter-scan exponent 1 ${exponent_ub} \
          --style none \
          --output inherit \
          --ignore-failure \
          "$EXEC --config IS --num_samples_exponent {exponent} --alpha ${alpha}  --beta ${beta}  --init 1 --horizon ${horizon} --seed 0 --sigma ${sigma} --theta ${theta}"
        #   --export-json "$here/${file_name}.json" \
        #   --export-csv  "$here/${file_name}.csv"  \
} 2>&1 | tee  ${here}/${file_name}.log.csv
