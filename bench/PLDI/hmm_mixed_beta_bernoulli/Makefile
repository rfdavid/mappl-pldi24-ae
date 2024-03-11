exponent = 15
alpha = 1
beta = 1
theta_0 = 0.3
theta_1 = 0.7

bench.is: bench.is.sh hmm_mixed.is.py
	sh bench.is.sh $(exponent) ${HORIZON} $(alpha) $(beta) $(theta_0) $(theta_1)

hmm_mixed.hoisted.mappl: hmm_mixed.mappl
	mappl var-elim hmm_mixed.mappl -output hmm_mixed.debug.mappl
	mappl var-elim hmm_mixed.mappl -hoist -output hmm_mixed.hoisted.mappl

hmm_mixed.mappl.py: hmm_mixed.hoisted.mappl header.py footer.py
	mappl dump-pyro hmm_mixed.hoisted.mappl | cat header.py - footer.py > hmm_mixed.mappl.py

bench.mappl: bench.mappl.sh hmm_mixed.mappl.py
	sh bench.mappl.sh $(exponent) ${HORIZON} $(alpha) $(beta) $(theta_0) $(theta_1)

bench.exact: bench.exact.sh hmm_mixed.mapplexact.py
	sh bench.exact.sh ${HORIZON} $(alpha) $(beta) $(theta_0) $(theta_1)

bench.plot: 
	python3 plot.py --horizon ${HORIZON} \
	--IS hmm_mixed.is.horizon_${HORIZON}-exponent_ub_$(exponent)-alpha_$(alpha)-beta_$(beta)-theta_$(theta_0)_$(theta_1).log.csv \
	--MAPPL hmm_mixed.mappl.horizon_${HORIZON}-exponent_ub_$(exponent)-alpha_$(alpha)-beta_$(beta)-theta_$(theta_0)_$(theta_1).log.csv \
	--EXACT hmm_mixed.exact.horizon_${HORIZON}-alpha_$(alpha)-beta_$(beta)-theta_$(theta_0)_$(theta_1).log.csv \

plot: bench.is bench.mappl bench.exact
	make bench.plot

clean:
	rm -f hmm_mixed.debug.mappl hmm_mixed.hoisted.mappl hmm_mixed.mappl.py
	rm -f *.log.csv
	rm -f *.log
	rm -f *.csv
	rm -f *.json
	rm -f *.fgg

.PHONY: clean bench.* hmm_mixed.hoisted.mappl *.py
