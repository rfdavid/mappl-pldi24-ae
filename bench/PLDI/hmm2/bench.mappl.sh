#!/bin/bash

hyperfine_runs=1

here=$(dirname "$0")
cd $here
wd=$(pwd)
echo "working dir:" $wd
EXEC="python3 -u hmm2.mappl.py"

sigma="${1:-0.3}"
theta0="${2:-0.3}"
theta1="${3:-0.7}"
theta="${theta0} ${theta1}"
horizon_ub="${4:-64}"

param_list="sigma_${sigma}-theta_${theta0}_${theta1}-horizon_ub_${horizon_ub}"
file_name="hmm2-mappl-${param_list}"

echo ${param_list}
# if [ -f ${file_name}.log.csv ]; then
#   echo "${file_name}.log.csv exists."
#   exit
# fi

echo
echo "running MAPPL"
echo

{
$EXEC --config header --sigma 0 --init 0 --horizon 0  --theta 0 0 
hyperfine --export-csv  "$here/${file_name}.csv"  \
          --runs "$hyperfine_runs" \
          --parameter-scan horizon 1 ${horizon_ub} \
          --style none \
          --output inherit \
          --ignore-failure \
          "$EXEC --config MAPPL --sigma ${sigma} --init 1 --horizon {horizon} --seed 0 --theta ${theta}"
          # --export-json "$here/${file_name}.json" \
} 2>&1 | tee  ${here}/${file_name}.log.csv
