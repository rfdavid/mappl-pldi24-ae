#!/usr/bin/env -S python3 -u

import sys
import argparse
import math
import pyro
import torch
import pyro.distributions as dist

sys.path.append("../../../pyro")
from utils import *

def print_header():
    print(
        f"{'config':>10}, "
        f"\t{'horizon':>7}, "
        f"\t{'time(secs)':>12}, "
        f"\t{'prob':>30}, "
        f"\t{'logprob':>37}, "
        f"\t{'init':>5}, "
        f"\t{'seed':>5}, "
        f"\t{'sigma':>15}, "
        f"\t{'theta0':>15}, "
        f"\t{'theta1':>15}"
    )

class HMM:
    def __init__(
            self,
            sigma=1,
            init=1,
            data=(1, ) * 32,
            theta=torch.tensor([0.9, 0.9]),
            seed=0
    ):
        self.sigma = sigma
        self.theta = theta
        self.init = init
        self.data = data
        self.seed = seed

    def model(self, cur, data):
        b = None
        if len(data) == 0:
            b = ()
        else:
            x = data[0]
            xs = data[1:]
            pyro.factor(f"obsve_{len(data)}", dist.Normal(cur, self.sigma).log_prob(float_tensor(x)))
            nxt = pyro.sample(f"state_{len(data)}", dist.Bernoulli(self.theta[cur])).long()
            self.model(nxt.item(), xs)
            b = ()
        return ()

    def model_evidence_ENUM(self):
        return model_evidence("enumerate", self.model, self.init, self.data)

    def one_trail(self, config, f):
        loginfo = "{:>10}, \t{:>7d}, \t{:>12.5f}, \t{:>30.20f}, \t{:>+37.30e}, "
        loginfo += "\t{:>5d}, " * 2
        loginfo += "\t{:>15.5f}, \t{:>15.5f}, \t{:>15.5f}"

        res = f()
        log_prob = res['return']
        time_used = res["time"]

        def none_to_int(n):
            if n is None:
                return -1
            else:
                return n
        seed = none_to_int(self.seed)

        print(
            loginfo.format(
                config,
                len(self.data),
                time_used,
                torch.exp(log_prob),
                log_prob,
                self.init,
                seed,
                self.sigma,
                self.theta[0],
                self.theta[1]
            )
        )

def main():

    parser = argparse.ArgumentParser()
    parser.add_argument('--config', choices=['ENUM', 'MAPPL', 'header'], required=True)
    parser.add_argument("--sigma", type=float, required=True)
    parser.add_argument("--init", type=int, required=True)
    parser.add_argument("--horizon", type=int, required=True)
    parser.add_argument("--seed", type=int)
    parser.add_argument("--theta", type=float, required=True, nargs=2)

    args = parser.parse_args()
    # args = parser.parse_args("--config MAPPL --sigma 0.3 --init 1 --horizon 1 --seed 0 --theta 0.3 0.7".split())
    # args = parser.parse_args("--config ENUM --sigma 0.3 --init 1 --horizon 1 --seed 0 --theta 0.3 0.7".split())

    hmma = HMM(
        sigma=args.sigma,
        init=args.init,
        data=(1, ) * args.horizon,
        theta=torch.tensor(args.theta),
        seed=args.seed
    )

    if args.config == "ENUM":
        hmma.one_trail(args.config, hmma.model_evidence_ENUM)
    elif args.config == "MAPPL":
        hmma.one_trail(args.config, hmma.model_evidence_MAPPL)
    elif args.config == "header":
        print_header()


if __name__ == '__main__':
    main()
