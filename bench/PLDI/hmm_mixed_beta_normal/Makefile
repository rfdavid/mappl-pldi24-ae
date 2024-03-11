exponent = 15

bench.is: bench.is.sh hmm_mixed.is.py
	sh bench.is.sh $(exponent) 16 1 1 0.125 0.3 0.7

hmm_mixed.hoisted.mappl: hmm_mixed.mappl
	mappl var-elim hmm_mixed.mappl -output hmm_mixed.debug.mappl
	mappl var-elim hmm_mixed.mappl -hoist -output hmm_mixed.hoisted.mappl

hmm_mixed.mappl.py: hmm_mixed.hoisted.mappl header.py footer.py
	mappl dump-pyro hmm_mixed.hoisted.mappl | cat header.py - footer.py > hmm_mixed.mappl.py

bench.mappl: bench.mappl.sh hmm_mixed.mappl.py
	sh bench.mappl.sh $(exponent) 16 1 1 0.125 0.3 0.7

bench.mapplwm: bench.mapplwm.sh hmm_mixed.mapplwm.py
	sh bench.mapplwm.sh 16 1 1 0.125 0.3 0.7

bench.plot:
	python3 plot.py --horizon 16 \
	--IS hmm_mixed.is.horizon_16-exponent_ub_$(exponent)-alpha_1-beta_1-sigma_0.125-theta_0.3_0.7.log.csv \
	--MAPPL hmm_mixed.mappl.horizon_16-exponent_ub_$(exponent)-alpha_1-beta_1-sigma_0.125-theta_0.3_0.7.log.csv \
	--EXACT hmm_mixed.mapplwm.horizon_16-exponent_ub_-alpha_1-beta_1-sigma_0.125-theta_0.3_0.7.log.csv

plot: bench.mapplwm bench.is bench.mappl
	make bench.plot
	
clean:
	rm -f hmm_mixed.debug.mappl hmm_mixed.hoisted.mappl hmm_mixed.mappl.py
	rm -f *.log.csv
	rm -f *.log
	rm -f *.csv
	rm -f *.json
	rm -f *.fgg

.PHONY: clean bench.* hmm_mixed.hoisted.mappl *.py
