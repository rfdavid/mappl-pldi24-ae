import sys
sys.path.append("../../../pyro")
from  utils import *
import torch
import pyro.distributions as dist
from functools import cache
import argparse


def hmm_mixed(_):
    return lambda_4(_)

@cache
def lambda_4(_):
    @cache
    def f_of_k(k):
        return lambda_3(k)
    return f_of_k(_)

@cache
def lambda_3(_):
    @cache
    def f_of_k(k):
        @cache
        def f_of_z(z):
            temp_0 = lambda_2(z)
            return temp_0(k)
        return f_of_z
    return f_of_k(_)

@cache
def lambda_2(_):
    @cache
    def f_of_z(z):
        @cache
        def f_of_k(k):
            @cache
            def f_of_data(data):
                temp_1 = len(data)
                if temp_1 == 0:
                    return k(0.0)
                else:
                    temp_2 = lambda_0(z)
                    temp_3 = temp_2(k)
                    k_of_z_prime = temp_3(data)
                    temp_4 = LogSumExp(k_of_z_prime)
                    temp_5 = temp_4(False)
                    temp_6 = temp_5(True)
                    temp_7 = lambda_1(z)
                    temp_8 = temp_7(data)
                    return temp_6 + temp_8
                
            return f_of_data
        return f_of_k
    return f_of_z(_)

@cache
def lambda_0(_):
    @cache
    def f_of_z(z):
        @cache
        def f_of_k(k):
            @cache
            def f_of_data(data):
                @cache
                def f_of_z_prime(z_prime):
                    temp_9 = bias(z)
                    temp_10 = hmm_mixed(k)
                    temp_11 = temp_10(z_prime)
                    temp_12 = tail(data)
                    temp_13 = temp_11(temp_12)
                    return dist.Bernoulli(temp_9).log_prob(torch.tensor(z_prime, dtype=torch.float)) \
                           + temp_13
                return f_of_z_prime
            return f_of_data
        return f_of_k
    return f_of_z(_)

@cache
def lambda_1(_):
    @cache
    def f_of_z(z):
        @cache
        def f_of_data(data):
            def model_0():
                p = pyro.sample(f"sample_0", dist.Beta(1, 1))
                pyro.factor(f"factor_0", dist.Normal(mean(p)(z), sigma(0.0)).log_prob(head(data)))
            global args, hmm
            if args.config == "MAPPL-IS":
                return logML(model_0)
            elif args.config == "MAPPL-WM":
                return hmm.transformed_p_WM(z, data[0])
            else:
                raise Exception("Wrong config")

        return f_of_data
    return f_of_z(_)

def head(data):
    return torch.tensor(data[0])

def tail(data):
    return data[1:]

def halt_transformed(_b):
    return 0.0

def mean(p):
    def f(z):
        return p+z
    return f

def bias(z):
    global hmm
    return hmm.theta[z]

def sigma(dummy):
    global hmm
    return torch.tensor(hmm.sigma)

def logML(f):
    global hmm
    return target(f, num_samples=hmm.num_samples)

class HMM:
    def __init__(
            self,
            num_samples_immediate=1000,
            num_samples_exponent=None,
            alpha=1,
            beta=1,
            init=1,
            data=(1, ) * 32,
            sigma=1,
            theta=torch.tensor([0.9, 0.9]),
            seed=0
    ):
        self.alpha = alpha
        self.beta = beta
        self.sigma = sigma
        self.theta = theta
        self.init = init
        self.data = data
        self.num_samples_immediate = num_samples_immediate
        self.num_samples_exponent = num_samples_exponent
        self.seed = seed
        self.transformed_p_IS.cache_clear()
        self.clean()

    @property
    def num_samples(self):
        assert self.num_samples_immediate is None or self.num_samples_exponent is None
        if self.num_samples_immediate is not None:
            return self.num_samples_immediate
        elif self.num_samples_exponent is not None:
            return 2 ** self.num_samples_exponent

    def transformed(*args):
        f = hmm_mixed
        for arg in args[1:]:
            f = f(arg)
        return f

    def transformed_p_WM(self, cur, x):
        # In[1]:=  Integrate[PDF[NormalDistribution[0+p,0.125], 0]PDF[BetaDistribution[1, 1],p], {p, 0, 1}]
        # Out[1]= 0.5
        # In[2]:=  Integrate[PDF[NormalDistribution[0+p,0.125], 1]PDF[BetaDistribution[1, 1],p], {p, 0, 1}]
        # Out[2]= 0.5
        # In[6]:= N[Integrate[PDF[NormalDistribution[1+p,0.125], 0]PDF[BetaDistribution[1, 1],p], {p, 0, 1}], 50]
        # Out[6]= 6.22096*^-16
        # In[5]:=  Integrate[PDF[NormalDistribution[1+p,0.125], 1]PDF[BetaDistribution[1, 1],p], {p, 0, 1}]
        # Out[5]= 0.5
        return torch.log(torch.tensor(6.22096e-16 if (cur == 1 and  x == 0) else 0.5))
    
    @cache
    def transformed_p_IS(self, cur, x):
        def _p():
            p = pyro.sample(f"beta", dist.Beta(float_tensor(self.alpha), float_tensor(self.beta)))
            pyro.factor(f"obs", dist.Normal(cur+p, self.sigma).log_prob(float_tensor(x)))
        if self.seed is not None:
            pyro.set_rng_seed(self.seed)
        return target(_p, num_samples=self.num_samples)

    def clean(self):
        if self.seed is not None:
            pyro.set_rng_seed(self.seed)

    def model_evidence_MAPPLIS(self):
        self.transformed_p = self.transformed_p_IS
        return model_evidence("transform", self.transformed, halt_transformed, self.init, self.data)

    def model_evidence_MAPPLIS(self):
        self.transformed_p = self.transformed_p_WM
        return model_evidence("transform", self.transformed, halt_transformed, self.init, self.data)
    
    def print_header(self):
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
            f"\t{'sigma':>8}, "
            f"\t{'theta0':>8}, "
            f"\t{'theta1':>8}"
        )

    def one_trail(self, config, f):
        loginfo = "{:>10}, \t{:>10d}, \t{:>7d}, \t{:>12.5f}, \t{:>30}, \t{:>+37.30e}, "
        loginfo += "\t{:>10d}, " * 2
        loginfo += "\t{:>5d}, " * 4
        loginfo += "\t{:>8.5f}, \t{:>8.5f}, \t{:>8.5f}"

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
                self.sigma,
                self.theta[0],
                self.theta[1]
            )
        )

def main():

    parser = argparse.ArgumentParser()
    parser.add_argument('--config', choices=['IS', 'MAPPL-IS', "MAPPL-WM", 'header'], required=True)
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
    parser.add_argument("--sigma", type=float, required=True)
    parser.add_argument("--theta", type=float, required=True, nargs=2)

    global hmm
    global args

    args = parser.parse_args()
    # args = parser.parse_args("--config MAPPL-IS --num_samples_exponent 3 --alpha 1  --beta 1 --init 1 --horizon 3 --seed 0 --num_samples_exponent 10 --sigma 0.3 --theta 0.9 0.8".split())
    # args = parser.parse_args("--config IS --num_samples_exponent 3 --alpha 1  --beta 1 --init 1 --horizon 3 --seed 0 --num_samples_exponent 10 --sigma 0.3 --theta 0.9 0.8".split())

    hmm = HMM(
        num_samples_exponent=args.num_samples_exponent,
        num_samples_immediate=args.num_samples_immediate,
        sigma=args.sigma,
        alpha=args.alpha,
        beta=args.beta,
        init=args.init,
        data=(1, ) * args.horizon,
        theta=torch.tensor(args.theta),
        seed=args.seed
    )

    if args.config == "IS":
        hmm.one_trail(args.config, hmm.model_evidence_IS)
    elif args.config == "MAPPL-IS":
        hmm.one_trail(args.config, hmm.model_evidence_MAPPLIS)
    elif args.config == "MAPPL-WM":
        hmm.one_trail(args.config, hmm.model_evidence_MAPPLIS)
    elif args.config == "header":
        hmm.print_header()

if __name__ == '__main__':
    main()