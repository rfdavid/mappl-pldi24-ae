#!/usr/bin/env -S python3 -u

import sys
import argparse
import math
import pyro
import torch
import pyro.distributions as dist

sys.path.append("../../../pyro")
from  utils import *

def beta_ME_exact_solution(alpha: int, beta: int, x: int, cur: int):
    n = float_tensor(alpha + beta + 1)
    m = beta + (1 - cur) if x == 0 else alpha + cur
    return m / n

def beta_ME_approx_solution(alpha:int, beta:int, x: int, cur: int, num_samples:int):
    def _beta():
        p = pyro.sample(f"beta", dist.Beta(alpha+cur, beta+1-cur))
        pyro.factor(f"obs", dist.Bernoulli(p).log_prob(float_tensor(x)))

    importance = Importance(_beta, guide=None, num_samples=num_samples)
    posterior = importance.run()
    me = posterior.get_log_normalizer()
    return me

def test_subproblem():
    alpha = 1
    beta  = 1
    exact = beta_ME_exact_solution(alpha, beta, x=1, cur=1)
    print(f"Ground truth: prob={exact:.20f}, log_prob={math.log(exact):.20f}")
    for n in range(1, 2):
        @timing
        def approx_timing():
            return beta_ME_approx_solution(alpha, beta, x=1, cur=1, num_samples=2**n)
        approx_dict = approx_timing()
        approx = approx_dict["return"]
        time = approx_dict["time"]
        print(f"n={n:3d}, \ttime = {time:12.5f} secs, \tnum_samples={2**n:10d} \tprob={torch.exp(approx):.20f}, \tlog_prob={approx:.20f}")

    # Ground truth: prob=0.66666666666666662966, log_prob=-0.40546510810816444037
    # n=  1, 	time = 0.00156 secs, 	num_samples=         2 	prob=0.70793593653036357427, 	log_prob=-0.34540167450904846191
    # n=  2, 	time = 0.00132 secs, 	num_samples=         4 	prob=0.82071254060858700452, 	log_prob=-0.19758236408233642578
    # n=  3, 	time = 0.00241 secs, 	num_samples=         8 	prob=0.58148489331749841913, 	log_prob=-0.54217028617858886719
    # n=  4, 	time = 0.00472 secs, 	num_samples=        16 	prob=0.75435204681585121822, 	log_prob=-0.28189611434936523438
    # n=  5, 	time = 0.00923 secs, 	num_samples=        32 	prob=0.64093555552562464506, 	log_prob=-0.44482636451721191406
    # n=  6, 	time = 0.01854 secs, 	num_samples=        64 	prob=0.66504341415812529004, 	log_prob=-0.40790295600891113281
    # n=  7, 	time = 0.03668 secs, 	num_samples=       128 	prob=0.64947555497538522751, 	log_prob=-0.43159008026123046875
    # n=  8, 	time = 0.07236 secs, 	num_samples=       256 	prob=0.67494047359408348985, 	log_prob=-0.39313077926635742188
    # n=  9, 	time = 0.14674 secs, 	num_samples=       512 	prob=0.67748390175186001816, 	log_prob=-0.38936948776245117188
    # n= 10, 	time = 0.31142 secs, 	num_samples=      1024 	prob=0.67059524303247697308, 	log_prob=-0.39958953857421875000
    # n= 11, 	time = 0.61611 secs, 	num_samples=      2048 	prob=0.66213911218734688990, 	log_prob=-0.41227960586547851562
    # n= 12, 	time = 1.23530 secs, 	num_samples=      4096 	prob=0.66628307014914511797, 	log_prob=-0.40604066848754882812
    # n= 13, 	time = 2.49166 secs, 	num_samples=      8192 	prob=0.66680209021943237513, 	log_prob=-0.40526199340820312500
    # n= 14, 	time = 4.94521 secs, 	num_samples=     16384 	prob=0.66982377507201862343, 	log_prob=-0.40074062347412109375
    # n= 15, 	time = 10.17075 secs, 	num_samples=     32768 	prob=0.66312777665370736813, 	log_prob=-0.41078758239746093750
    # n= 16, 	time = 21.09696 secs, 	num_samples=     65536 	prob=0.66629164833456000494, 	log_prob=-0.40602779388427734375
    # n= 17, 	time = 42.77806 secs, 	num_samples=    131072 	prob=0.66688349192722595760, 	log_prob=-0.40513992309570312500
    # n= 18, 	time = 84.85355 secs, 	num_samples=    262144 	prob=0.66704823359361609114, 	log_prob=-0.40489292144775390625
    # n= 19, 	time = 172.13027 secs, 	num_samples=    524288 	prob=0.66626813801576112439, 	log_prob=-0.40606307983398437500
    # n= 20, 	time = 343.27900 secs, 	num_samples=   1048576 	prob=0.66653569630753606301, 	log_prob=-0.40566158294677734375
    # n= 21, 	time = 689.58515 secs, 	num_samples=   2097152 	prob=0.66682371157890329982, 	log_prob=-0.40522956848144531250
    # n= 22, 	time = 1598.51118 secs, 	num_samples=   4194304 	prob=0.66668064208350064082, 	log_prob=-0.40544414520263671875

def print_header():
    print(
        f"{'config':>10}, "
        f"\t{'nsamples':>10}, "
        f"\t{'horizon':>7}, "
        f"\t{'time(secs)':>12}, "
        f"\t{'prob':>30}, "
        f"\t{'logprob':>37}, "
        f"\t{'exponent':>10}, "
        f"\t{'immediate':>10}, "
        f"\t{'alpha':>5}, "
        f"\t{'beta':>5}, "
        f"\t{'init':>5}, "
        f"\t{'seed':>5}, "
        f"\t{'theta0':>8}, "
        f"\t{'theta1':>8}"
    )

class HMMaMixture:
    def __init__(
            self,
            num_samples_immediate=1000,
            num_samples_exponent=None,
            alpha=1,
            beta=1,
            init=1,
            data=(1, ) * 32,
            theta=torch.tensor([0.9, 0.9]),
            seed=0
    ):
        self.alpha = alpha
        self.beta = beta
        self.theta = theta
        self.init = init
        self.data = data
        self.num_samples_immediate = num_samples_immediate
        self.num_samples_exponent = num_samples_exponent
        self.seed = seed
        self.transformed_p_IS.cache_clear()
        self.transformed_nxt.cache_clear()

    @property
    def num_samples(self):
        assert self.num_samples_immediate is None or self.num_samples_exponent is None
        if self.num_samples_immediate is not None:
            return self.num_samples_immediate
        elif self.num_samples_exponent is not None:
            return 2 ** self.num_samples_exponent

    def model(self, cur, data):
        b = None
        if len(data) == 0:
            b = ()
        else:
            x = data[0]
            xs = data[1:]
            p = pyro.sample(f"beta_{len(data)}", dist.Beta(self.alpha+cur,self.beta+1-cur))
            pyro.factor(f"obs_{len(data)}", dist.Bernoulli(p).log_prob(float_tensor(x)))
            nxt = pyro.sample(f"state_{len(data)}", dist.Bernoulli(self.theta[cur])).long()
            b = self.model(nxt.item(), xs)
        return ()

    @cache
    def transformed_p_IS(self, cur, x):
        def _p():
            p = pyro.sample(f"beta", dist.Beta(float_tensor(self.alpha+cur), float_tensor(self.beta+1-cur)))
            pyro.factor(f"obs", dist.Bernoulli(p).log_prob(float_tensor(x)))
        if self.seed is not None:
            pyro.set_rng_seed(self.seed)
        return target(_p, num_samples=self.num_samples)

    def transformed_p_exact(self, cur, x):
        return torch.log(beta_ME_exact_solution(alpha=self.alpha, beta=self.beta, x=x, cur=cur))

    @cache
    def transformed_nxt(self, cur, xs):
        def _nxt(nxt, cur, xs):
            pyro.sample(f"state", dist.Bernoulli(self.theta[cur]), obs=float_tensor(nxt))
            pyro.factor(f"call", self.transformed(halt_transformed, nxt, xs))
        log_probs = torch.tensor([target(_nxt, nxt, cur, xs) for nxt in [0,1]])
        return torch.logsumexp(log_probs, dim=-1)

    def transformed(self, k, cur, data):
        def _body():
            def transformed_then(k):
                def _then():
                    pyro.factor(f"k_then", k())
                return target(_then)
            def transformed_else(k):
                x = data[0]
                xs = data[1:]
                def _else():
                    pyro.factor(f"p", self.transformed_p(cur, x))
                    pyro.factor(f"k_else", k())
                    pyro.factor(f"nxt", self.transformed_nxt(cur, xs))
                return target(_else)

            pyro.factor("k_body", k())
            pyro.factor("*", transformed_then(halt_transformed) if len(data) == 0 else transformed_else(halt_transformed))
        return target(_body)

    def clean(self):
        if self.seed is not None:
            pyro.set_rng_seed(self.seed)
        self.transformed_p_IS.cache_clear()
        self.transformed_nxt.cache_clear()

    def model_evidence_IS(self):
        return model_evidence("importance", self.model, self.init, self.data, num_samples=self.num_samples)

    def model_evidence_MAPPLSI(self):
        self.transformed_p = self.transformed_p_exact
        return model_evidence("transform", self.transformed, halt_transformed, self.init, self.data)

    def model_evidence_MAPPLIS(self):
        self.transformed_p = self.transformed_p_IS
        return model_evidence("transform", self.transformed, halt_transformed, self.init, self.data)

    def one_trail(self, config, f):
        loginfo = "{:>10}, \t{:>10d}, \t{:>7d}, \t{:>12.5f}, \t{:>30.20f}, \t{:>+37.30e}, "
        loginfo += "\t{:>10d}, " * 2
        loginfo += "\t{:>5d}, " * 4
        loginfo += "\t{:>8.5f}, \t{:>8.5f}"

        self.clean()
        res = f()
        log_prob = res['return']
        time_used = res["time"]
        def none_to_int(n):
            if n is None:
                return -1
            else:
                return n

        n_exp = none_to_int(self.num_samples_exponent)
        n_imd = none_to_int(self.num_samples_immediate)
        n_sample = none_to_int(self.num_samples)
        seed = none_to_int(self.seed)
        print(
            loginfo.format(
                config,
                n_sample,
                len(self.data),
                time_used,
                torch.exp(log_prob),
                log_prob,
                n_exp,
                n_imd,
                self.alpha,
                self.beta,
                self.init,
                seed,
                self.theta[0],
                self.theta[1]
            )
        )

    def run(self, n_range, exponent_range):
        for n in n_range:
            loginfo = "{:12}, " \
                      "\tn={:3d}, " \
                      "\texponent={:3d}, " \
                      "\tnum_samples={:10d}, " \
                      "\ttime = {:12.5f} secs, " \
                      "\tprob={:30.20f}, " \
                      "\tlog_prob={:.30e}, "
            self.data = (1, ) * n
            for exponent in exponent_range:

                self.num_samples = 2**exponent

                self.clean(seed=0)
                res = model_evidence("importance", self.model, self.init, self.data, num_samples=self.num_samples)
                print(
                    loginfo.format(
                        "importance", n, exponent, self.num_samples, res['time'], torch.exp(res['return']), res['return']
                    )
                )

                self.clean(seed=0)
                self.transformed_p = self.transformed_p_exact
                res = model_evidence("transform", self.transformed, halt_transformed, self.init, self.data)
                print(
                    loginfo.format(
                        "MAPPL-SI", n, exponent, self.num_samples, res['time'], torch.exp(res['return']), res['return']
                    )
                )

                self.clean(seed=0)
                self.transformed_p = self.transformed_p_IS
                res = model_evidence("transform", self.transformed, halt_transformed, self.init, self.data)
                print(
                    loginfo.format(
                        "MAPPL-IS", n, exponent, self.num_samples, res['time'], torch.exp(res['return']), res['return']
                    )
                )

def main():

    parser = argparse.ArgumentParser()
    parser.add_argument('--print_header', action='store_true')
    parser.add_argument('--config', choices=['IS', 'MAPPL-IS', 'MAPPL-SI'], required=True)
    group = parser.add_mutually_exclusive_group()
    group.add_argument('--num_samples_immediate', type=int)
    group.add_argument(
        '--num_samples_exponent', type=int,
        help="use 2^%(dest)s many samples"
    )
    parser.add_argument("--alpha", type=int, required=True)
    parser.add_argument("--beta", type=int, required=True)
    parser.add_argument("--init", type=int, required=True)
    parser.add_argument("--horizon", type=int, required=True)
    parser.add_argument("--seed", type=int)
    parser.add_argument("--theta", type=float, required=True, nargs=2)

    args = parser.parse_args()
    # args = parser.parse_args("--config MAPPL-SI --alpha 1  --beta 1 --init 1 --horizon 1 --seed 0 --num_samples_exponent 10 --theta 0.9 0.8".split())
    # args = parser.parse_args("--config IS --num_samples_exponent 3 --alpha 1  --beta 1 --init 1 --horizon 1 --seed 0 --num_samples_exponent 10 --theta 0.9 0.8".split())
    # args = parser.parse_args("--config MAPPL-SI --print_header --alpha 0  --beta 0 --init 0 --horizon 0  --theta 0 0".split())

    if args.print_header == True:
        print_header()
        return

    hmma = HMMaMixture(
        num_samples_exponent=args.num_samples_exponent,
        num_samples_immediate=args.num_samples_immediate,
        alpha=args.alpha,
        beta=args.beta,
        init=args.init,
        data=(1, ) * args.horizon,
        theta=torch.tensor(args.theta),
        seed=args.seed
    )
    if args.config == "IS":
        hmma.one_trail(args.config, hmma.model_evidence_IS)
    elif args.config == "MAPPL-IS":
        hmma.one_trail(args.config, hmma.model_evidence_MAPPLIS)
    elif args.config == "MAPPL-SI":
        hmma.one_trail(args.config, hmma.model_evidence_MAPPLSI)

if __name__ == '__main__':
    main()
