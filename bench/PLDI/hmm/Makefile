enum_len = 16
mappl_len = 64

hmm.hoisted.mappl: hmm.mappl
	mappl var-elim hmm.mappl -output hmm.debug.mappl
	mappl var-elim hmm.mappl -hoist -output hmm.hoisted.mappl

hmm.mappl.py: hmm.hoisted.mappl header.py footer.py
	mappl dump-pyro hmm.hoisted.mappl | cat header.py - footer.py > hmm.mappl.py

bench.enum: bench.enum.sh hmm.enum.py
	sh bench.enum.sh 0.3 0.3 0.7 $(enum_len)

bench.mappl: bench.enum.sh hmm.mappl.py
	sh bench.mappl.sh 0.3 0.3 0.7 $(mappl_len)

bench.perpl: bench.perpl.sh
	sh bench.perpl.sh $(mappl_len)

bench.plot:
	python3 plot.py \
	--ENUM hmm-enum-sigma_0.3-theta_0.3_0.7-horizon_ub_$(enum_len).csv \
	--MAPPL hmm-mappl-sigma_0.3-theta_0.3_0.7-horizon_ub_$(mappl_len).csv \
    --PERPL hmm-perpl.csv

plot: bench.enum bench.mappl bench.perpl
	make bench.plot

clean:
	rm -f hmm.debug.mappl hmm.hoisted.mappl hmm.mappl.py
	rm -f *.log.csv
	rm -f *.log
	rm -f *.csv
	rm -f *.json
	rm -f *.fgg
	rm -f *.png

.PHONY: clean data *.py
