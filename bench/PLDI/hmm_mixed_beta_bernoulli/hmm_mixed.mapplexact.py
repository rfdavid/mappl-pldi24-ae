import sys
sys.path.append("../../../pyro")
from  utils import *
import torch
import pyro.distributions as dist
from functools import cache
import argparse


def hmm_mixed(k, z, data):
    temp_0 = len(data)
    if temp_0 == 0:
        return k(0.0)
    else:
        k_of_z_prime = partial(lambda_0, data, k, z)
        temp_1 = LogSumExp(k_of_z_prime, False, True)
        temp_2 = logML_1(data, z)
        return temp_1 + temp_2
    

@cache
def lambda_0(data, k, z, z_prime):
    temp_3 = bias(z)
    temp_4 = tail(data)
    temp_5 = hmm_mixed(k, z_prime, temp_4)
    return dist.Bernoulli(temp_3).log_prob(torch.tensor(z_prime, dtype=torch.float)) \
           + temp_5
@cache
def logML_1(data, z):
    global args, hmm
    assert args.config == "MAPPL-SI"
    return hmm.transformed_p_exact(z, data[0])

def head(data):
    return torch.tensor(data[0], dtype=float)

def tail(data):
    return data[1:]

def alpha(_):
    return 1

def beta(_):
    return 1

def to_nat(x):
    return int(x)

def safe_sub(x):
    def _sub(y):
        return x - y
    return _sub

def halt_transformed(_b):
    return 0.0

def mean(p):
    def f(z):
        return p+z
    return f

def bias(z):
    global hmm
    return hmm.theta[z]

def logML(f):
    global hmm
    return target(f, num_samples=hmm.num_samples)
    
def beta_ME_exact_solution(alpha: int, beta: int, x: int, cur: int):
    n = float_tensor(alpha + beta + 1)
    m = beta + (1 - cur) if x == 0 else alpha + cur
    return m / n

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
    
    def transformed_p_exact(self, cur, x):
        return torch.log(beta_ME_exact_solution(alpha=self.alpha, beta=self.beta, x=x, cur=cur))

    def model_evidence_IS(self):
        return model_evidence("importance", self.model, self.init, self.data, num_samples=self.num_samples)

    def model_evidence_MAPPLSI(self):
        return model_evidence("transform", hmm_mixed, halt_transformed, self.init, self.data)

    def model_evidence_MAPPLIS(self):
        return model_evidence("transform", hmm_mixed, halt_transformed, self.init, self.data)

    def one_trail(self, config, f):
        loginfo = "{:>10}, \t{:>10d}, \t{:>7d}, \t{:>12.5f}, \t{:>30.20f}, \t{:>+37.30e}, "
        loginfo += "\t{:>10d}, " * 2
        loginfo += "\t{:>5d}, " * 4
        loginfo += "\t{:>8.5f}, \t{:>8.5f}"

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

    global args
    args = parser.parse_args()
    # args = parser.parse_args("--config MAPPL-SI --alpha 1  --beta 1 --init 1 --horizon 1 --seed 0 --num_samples_exponent 10 --theta 0.9 0.8".split())
    # args = parser.parse_args("--config IS --num_samples_exponent 3 --alpha 1  --beta 1 --init 1 --horizon 1 --seed 0 --num_samples_exponent 10 --theta 0.9 0.8".split())
    # args = parser.parse_args("--config MAPPL-SI --print_header --alpha 0  --beta 0 --init 0 --horizon 0  --theta 0 0".split())

    if args.print_header == True:
        print_header()
        return

    global hmm
    hmm = HMMaMixture(
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
        hmm.one_trail(args.config, hmm.model_evidence_IS)
    elif args.config == "MAPPL-IS":
        hmm.one_trail(args.config, hmm.model_evidence_MAPPLIS)
    elif args.config == "MAPPL-SI":
        hmm.one_trail(args.config, hmm.model_evidence_MAPPLSI)

if __name__ == '__main__':
    main()
