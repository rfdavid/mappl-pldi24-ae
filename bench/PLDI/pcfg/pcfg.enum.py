#!/usr/bin/env -S python3 -u

import sys
import argparse
import math
import pyro
import torch
import pyro.distributions as dist
from functools import partial

sys.setrecursionlimit(32767)
sys.path.append("../../../pyro")
from utils import *

class PCFG:
    def __init__(
            self,
            stop_prob=0.9,
            data=(1, ) * 32,
    ):
        self.stop_prob = stop_prob
        self.data = data

    def model(self, w, prefix):
        
        if (len(w) == 0):
            pyro.factor(f"{prefix}/w", -torch.inf)
            return ()

        stop = pyro.sample(f"{prefix}_stop", dist.Bernoulli(self.stop_prob))
        if stop:
            b0 = w[1:]
        else:
            if (len(w) <= 1):
                pyro.factor(f"{prefix}/w", -torch.inf)
                return ()
            u = self.model(w[:-1],f"{prefix}/d1")
            v = self.model((*u, w[-1]),f"{prefix}/d2")
            b0 = v
        return b0

    def entry(self, w):
        u = self.model(w, "")
        if not len(u) == 0:
            pyro.factor("end", -torch.inf)

    def model_evidence_ENUM(self):
        return model_evidence("enumerate", self.entry, self.data)

    def entry_k(self, u):
        return torch.zeros(1) if len(u) == 0 else -torch.inf 

    def print_header(self):
        print(
            f"{'config':>10}, "
            f"\t{'stopprob':>15}, "
            f"\t{'length':>7}, "
            f"\t{'time(secs)':>12}, "
            f"\t{'logprob':>37}, "
            f"\t{'prob':>30}"
        )
    def one_trail(self, config, f):
        loginfo = "{:>10}, \t{:>15.5f}, \t{:>7d}, \t{:>12.5f},\t{:>+37.30e}, \t{:>30}"
        res = f()
        log_prob = res['return']
        time_used = res["time"]

        print(
            loginfo.format(
                config,
                self.stop_prob,
                len(self.data),
                time_used,
                log_prob,
                torch.exp(log_prob)
            )
        )

def main():

    parser = argparse.ArgumentParser()
    parser.add_argument('--config', choices=['ENUM', 'MAPPL', 'header'], required=True)
    parser.add_argument("--stop_prob", type=float, required=True)
    parser.add_argument("--length", type=int, required=True)

    args = parser.parse_args()
    # args = parser.parse_args("--config ENUM --stop_prob=0.5 --length=3".split())
    # args = parser.parse_args("--config MAPPL --stop_prob=0.5 --length=1".split())

    pcfg = PCFG(
        stop_prob=args.stop_prob,
        data=(0, ) * args.length,
    )
    if args.config == "ENUM":
        pcfg.one_trail(args.config, pcfg.model_evidence_ENUM)
    elif args.config == "MAPPL":
        pcfg.one_trail(args.config, pcfg.model_evidence_MAPPL)
    elif args.config == "header":
        pcfg.print_header()

if __name__ == '__main__':
    main()
