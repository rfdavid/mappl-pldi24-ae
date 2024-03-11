enum_len = 9
base_len = 14
dice_len = 20
perpl_len = 120
mappl_len = 120

pcfg.hoisted.mappl: pcfg.mappl
	mappl var-elim pcfg.mappl -output pcfg.debug.mappl
	mappl var-elim pcfg.mappl -hoist -output pcfg.hoisted.mappl

pcfg.mappl.py: pcfg.hoisted.mappl header.py footer.py
	mappl dump-pyro pcfg.hoisted.mappl | cat header.py - footer.py > pcfg.mappl.py

pcfg.base.mappl.py: pcfg.base.mappl
	mappl hoist pcfg.base.mappl -output pcfg.base.hoisted.mappl
	mappl dump-pyro pcfg.base.hoisted.mappl | cat header.py - footer.py > pcfg.base.mappl.py

bench.enum: bench.enum.sh pcfg.enum.py
	sh bench.enum.sh ENUM $(enum_len) ${STOPPROB}

bench.mappl: bench.mappl.sh pcfg.mappl.py
	sh bench.mappl.sh MAPPL $(mappl_len) ${STOPPROB}

bench.perpl: bench.perpl.sh pcfg.perpl.pl
	sh bench.perpl.sh $(perpl_len) ${STOPPROB}

bench.dice: bench.dice.sh pcfg.dice.pl
	sh bench.dice.sh ${dice_len} ${STOPPROB}

bench.base: bench.base.sh pcfg.base.mappl.py
	sh bench.base.sh MAPPL $(base_len) ${STOPPROB}

bench.plot:
	python3 plot.py \
	--ENUM PCFG-config_ENUM-stop_prob_${STOPPROB}.csv \
	--BASE base-config_MAPPL-stop_prob_${STOPPROB}.csv \
	--MAPPL mappl-config_MAPPL-stop_prob_${STOPPROB}.csv \
	--PERPL pcfg-perpl.csv \
	--DICE pcfg-dice-stop_prob_${STOPPROB}.csv \
	--prob ${STOPPROB}

plot: bench.enum bench.mappl bench.perpl bench.base bench.dice
	make bench.plot

clean:
	rm -f pcfg.debug.mappl pcfg.hoisted.mappl pcfg.mappl.py pcfg.base.mappl.py pcfg.base.hoisted.mappl
	rm -f *.log.csv
	rm -f *.log
	rm -f *.csv
	rm -f *.json
	rm -f *.fgg
	rm -f *.png

.PHONY: clean bench.* *.py
