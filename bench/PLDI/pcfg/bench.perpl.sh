#!/bin/bash

hyperfine_runs=1 

perpl_range="${1:-64}" # default: 64
stop_prob=${2}

echo "perpl_range = ${perpl_range}"
echo "stop_prob = ${stop_prob}"

here=$(dirname "$0")

perplc="perplc"
sum_product="sum_product.py"

file_name="pcfg-perpl"
fgg_file="$here/${file_name}-{length}.fgg"

{
hyperfine --export-csv  "$here/${file_name}.csv"  \
          --runs "$hyperfine_runs" \
          --parameter-scan length 1 "$perpl_range" \
          --setup "perl $here/pcfg.perpl.pl {length} ${stop_prob} | $perplc > ${fgg_file}" \
          --cleanup "rm ${fgg_file}" \
          --style none --output inherit \
          --ignore-failure \
        "$sum_product -d -m fixed-point -l 1e-50 ${fgg_file}" 
        # --export-json "$here/${file_name}.json" \
} 2>&1 | tee "$here/pcfg.perpl.log"
