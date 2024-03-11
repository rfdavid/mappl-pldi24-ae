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
    
def beta_ME_approx_solution(alpha:int, beta:int, x: int, cur: int, num_samples:int):
    def _beta():
        p = pyro.sample(f"beta", dist.Beta(alpha+cur, beta+1-cur))
        pyro.factor(f"obs", dist.Bernoulli(p).log_prob(float_tensor(x)))

    importance = Importance(_beta, guide=None, num_samples=num_samples)
    posterior = importance.run()
    me = posterior.get_log_normalizer()
    return me

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
    
    def transformed(*args):
        f = hmm_mixed
        for arg in args[1:]:
            f = f(arg)
        return f

    def model_evidence_IS(self):
        return model_evidence("importance", self.model, self.init, self.data, num_samples=self.num_samples)

    def model_evidence_MAPPLIS(self):
        return model_evidence("transform", self.transformed, halt_transformed, self.init, self.data)

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
