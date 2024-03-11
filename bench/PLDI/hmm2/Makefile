enum_len = 16
mappl_len = 64

hmm2.hoisted.mappl: hmm2.mappl
	mappl var-elim hmm2.mappl -output hmm2.debug.mappl
	mappl var-elim hmm2.mappl -hoist -output hmm2.hoisted.mappl

hmm2.mappl.py: hmm2.hoisted.mappl header.py footer.py
	mappl dump-pyro hmm2.hoisted.mappl | cat header.py - footer.py > hmm2.mappl.py

bench.enum: bench.enum.sh hmm2.enum.py
	sh bench.enum.sh 0.3 0.3 0.7 $(enum_len)

bench.mappl: bench.enum.sh hmm2.mappl.py
	sh bench.mappl.sh 0.3 0.3 0.7 $(mappl_len)

bench.perpl: bench.perpl.sh
	sh bench.perpl.sh $(mappl_len)

bench.plot: 
	python3 plot.py \
	--ENUM hmm2-enum-sigma_0.3-theta_0.3_0.7-horizon_ub_$(enum_len).csv \
	--MAPPL hmm2-mappl-sigma_0.3-theta_0.3_0.7-horizon_ub_$(mappl_len).csv \
    --PERPL hmm2-perpl.csv

plot: bench.enum bench.mappl bench.perpl
	make bench.plot

clean:
	rm -f hmm2.debug.mappl hmm2.hoisted.mappl hmm2.mappl.py
	rm -f *.log.csv
	rm -f *.log
	rm -f *.csv
	rm -f *.json
	rm -f *.fgg 
	rm -f *.png

.PHONY: clean bench.* *.py
