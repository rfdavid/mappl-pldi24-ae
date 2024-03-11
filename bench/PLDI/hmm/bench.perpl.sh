#!/bin/bash

hyperfine_runs=1 

perpl_range="${1:-64}" # default: 64

echo "perpl_range = ${perpl_range}"

here=$(dirname "$0")

perplc="perplc"
sum_product="sum_product.py"

file_name="hmm-perpl"
fgg_file="$here/${file_name}-{length}.fgg"

{
hyperfine --export-csv  "$here/${file_name}.csv"  \
          --runs "$hyperfine_runs" \
          --parameter-scan length 1 "$perpl_range" \
          --setup "perl $here/hmm.perpl.pl {length} | $perplc > ${fgg_file}" \
          --cleanup "rm ${fgg_file}" \
          --style none --output inherit \
          --ignore-failure \
        "$sum_product -d -m fixed-point -l 1e-50 ${fgg_file}" 
        # --export-json "$here/${file_name}.json" \
 } 2>&1 | tee "$here/hmm.perpl.log"
