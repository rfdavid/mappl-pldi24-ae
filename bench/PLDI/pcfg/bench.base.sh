#!/bin/bash

# Number of times hyperfine runs each sample before averaging
hyperfine_runs=1 # default: 10

here=$(dirname "$0")
cd $here
wd=$(pwd)
echo "working dir:" $wd
MAPPL="python3 -u pcfg.base.mappl.py"

if [ $# -eq 0 ]
  then
    echo "No arguments supplied"
    exit 1
fi

config="${1}"
length_ub="${2:-100}"
stop_prob="${3:-0.5}"

param_list="config_${config}-stop_prob_${stop_prob}"
file_name="base-${param_list}"

###########################################
{
$MAPPL --config header --length -1  --stop_prob -1
hyperfine --export-csv  "$here/${file_name}.csv"  \
          --runs "$hyperfine_runs" \
          --parameter-scan length 1 ${length_ub} \
          --style none \
          --output inherit \
          --ignore-failure \
          "$MAPPL --config ${config} --stop_prob ${stop_prob} --length {length}"
} 2>&1 | tee  ${here}/${file_name}.log.csv
