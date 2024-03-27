#!/bin/bash

make -C hmm plot
make -C hmm2 plot
STOPPROB=0.5 make -C pcfg plot
STOPPROB=0.9 make -C pcfg plot
HORIZON=32 make -C hmm_mixed_beta_bernoulli plot
HORIZON=64 make -C hmm_mixed_beta_bernoulli plot
make -C hmm_mixed_beta_normal plot
