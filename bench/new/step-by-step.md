# Step-by-step instructions on how to create new programs and compile them to Pyro

1. Enter nix shell.

    ```
    (base) ┌─[xxx@xxx (SHLVL=1)] - [~] - [Wed Apr 03, 14:05]
    └─[$] <> docker run -it mappl-pldi24-ae-tested bash -c "cd /mappl-pldi24-ae/bench && nix --extra-experimental-features nix-command --extra-experimental-features flakes develop --command fish" 
    WARNING: The requested image's platform (linux/amd64) does not match the detected host platform (linux/arm64/v8) and no specific platform was requested
    path '/mappl-pldi24-ae/bench' does not contain a 'flake.nix', searching up
    warning: Git tree '/mappl-pldi24-ae' is dirty
    Welcome to fish, the friendly interactive shell
    Type help for instructions on how to use fish
    root@f0eb7fdfbe8e /m/bench (docker)# 
    ```

2. Create a new program.

    ```
    root@f0eb7fdfbe8e /m/bench (docker)# echo -e "external def f : bool -> ureal\ndef coin(obs:bool) : Unit = {\n\tx = root@f0eb7fdfbe8e /m/bench (docker)# echo -e "external def p : bool -> ureal\ndef coin(obs:bool) : Unit = {\n\tfair = sample(BERN(0.2));\n\tfactor(logPr BERN(p(fair)) at obs end)\n}\n" > coin.mappl

    root@f0eb7fdfbe8e /m/bench (docker)# cat coin.mappl
    external def p : bool -> ureal
    def coin(obs:bool) : Unit = {
	    fair = sample(BERN(0.2));
	    factor(logPr BERN(p(fair)) at obs end)
    }
    ```

3. Apply variable elimnation.

    `var-elim` without `-hoisted` flag generates a more readable progrm.
    ```
    root@f0eb7fdfbe8e /m/bench (docker)# mappl var-elim coin.mappl 
    external def p : bool -> ureal
    def coin : (Unit -> real) -> bool -> real =
        fun (k : Unit -> real) =>
            fun (obs : bool) =>
                let k_of_fair = fun (fair : bool) =>
                    logPr BERN(0.200000) at fair end +
                    logPr BERN((p fair)) at obs end + (k unit)
                in
                (((LogSumExp k_of_fair) false) true)
                end
    ```

    But `-hoisted` is requried for efficiency. 
    ```
    root@f0eb7fdfbe8e /m/bench (docker)# mappl var-elim -hoist coin.mappl > coin.hoisted.mappl
    root@f0eb7fdfbe8e /m/bench (docker)# cat coin.hoisted.mappl 
    external def p : bool -> ureal
    def coin : (Unit -> real) -> bool -> real =
        lambda_2

    def lambda_2 : (Unit -> real) -> bool -> real =
        fun (k : Unit -> real) => (lambda_1 k)

    def lambda_1 : (Unit -> real) -> bool -> real =
        fun (k : Unit -> real) =>
            fun (obs : bool) =>
                let k_of_fair = ((lambda_0 obs) k) in
                (((LogSumExp k_of_fair) false) true)
                end

    def lambda_0 : bool -> (Unit -> real) -> bool -> real =
        fun (obs : bool) =>
            fun (k : Unit -> real) =>
                fun (fair : bool) =>
                    logPr BERN(0.200000) at fair end +
                    logPr BERN((p fair)) at obs end + (k unit)
    ```

4. Compile the hoisted program to Pyro.
    ```
    root@f0eb7fdfbe8e /m/bench (docker)# mappl dump-pyro coin.hoisted.mappl 
    import torch
    import pyro.distributions as dist
    from functools import cache
    import argparse

    def coin(_):
        return lambda_2(_)

    @cache
    def lambda_2(_):
        @cache
        def f_of_k(k):
            return lambda_1(k)
        return f_of_k(_)

    @cache
    def lambda_1(_):
        @cache
        def f_of_k(k):
            @cache
            def f_of_obs(obs):
                temp_0 = lambda_0(obs)
                k_of_fair = temp_0(k)
                temp_1 = LogSumExp(k_of_fair)
                temp_2 = temp_1(False)
                return temp_2(True)
            return f_of_obs
        return f_of_k(_)

    @cache
    def lambda_0(_):
        @cache
        def f_of_obs(obs):
            @cache
            def f_of_k(k):
                @cache
                def f_of_fair(fair):
                    temp_3 = p(fair)
                    temp_4 = k(0.0)
                    return dist.Bernoulli(0.200000).log_prob(torch.tensor(fair, dtype=torch.float)) \
                        + dist.Bernoulli(temp_3).log_prob(torch.tensor(obs, dtype=torch.float)) \
                        + temp_4
                return f_of_fair
            return f_of_k
        return f_of_obs(_)
    ```

5. Define the external function `external def p : bool -> ureal` and other necessities (or import from [here](https://github.com/mappl-pldi24-ae/mappl-pldi24-ae/blob/mappl-pldi24-ae/pyro/utils.py)) in a python script `footer.py`.
    ```
    root@f0eb7fdfbe8e /m/bench (docker)# echo -e "\
    def p(fair):\n\
        if fair:\n\
            return 0.5\n\
        else:\n\
            return 0.2\n\
    \n\
    def halt(*args, **kwargs):\n\
        return 0.0\n\
    \n\
    def LogSumExp(f):\n\
        def lower(lo):\n\
            def upper(hi):\n\
                int_lo = int(lo)\n\
                int_hi = int(hi)\n\
                log_probs = torch.tensor([f(z) for z in range(int_lo, int_hi+1)])\n\
                return torch.logsumexp(log_probs, dim=-1)\n\
            return upper\n\
        return lower\n\
    \n\
    if __name__ == '__main__':\n\
        log_prob_false = coin(halt)(False)\n\
        log_prob_true  = coin(halt)(True)\n\
        print(log_prob_false.exp().item())\n\
        print(log_prob_true.exp().item())\n\
    " > footer.py

    root@f0eb7fdfbe8e /m/bench (docker)# cat footer.py 
    def p(fair):
        if fair:
            return 0.5
        else:
            return 0.2

    def halt(*args, **kwargs):
        return 0.0

    def LogSumExp(f):
        def lower(lo):
            def upper(hi):
                int_lo = int(lo)
                int_hi = int(hi)
                log_probs = torch.tensor([f(z) for z in range(int_lo, int_hi+1)])
                return torch.logsumexp(log_probs, dim=-1)
            return upper
        return lower

    if __name__ == '__main__':
        log_prob_false = coin(halt)(False)
        log_prob_true  = coin(halt)(True)
        print(log_prob_false.exp().item())
        print(log_prob_true.exp().item())
    ```

6. Glue compiled program and `footer.py` together.
    ```
    root@f0eb7fdfbe8e /m/bench (docker)# mappl dump-pyro coin.hoisted.mappl | cat - footer.py 
    import torch
    import pyro.distributions as dist
    from functools import cache
    import argparse


    def coin(_):
        return lambda_2(_)

    @cache
    def lambda_2(_):
        @cache
        def f_of_k(k):
            return lambda_1(k)
        return f_of_k(_)

    @cache
    def lambda_1(_):
        @cache
        def f_of_k(k):
            @cache
            def f_of_obs(obs):
                temp_0 = lambda_0(obs)
                k_of_fair = temp_0(k)
                temp_1 = LogSumExp(k_of_fair)
                temp_2 = temp_1(False)
                return temp_2(True)
            return f_of_obs
        return f_of_k(_)

    @cache
    def lambda_0(_):
        @cache
        def f_of_obs(obs):
            @cache
            def f_of_k(k):
                @cache
                def f_of_fair(fair):
                    temp_3 = p(fair)
                    temp_4 = k(0.0)
                    return dist.Bernoulli(0.200000).log_prob(torch.tensor(fair, dtype=torch.float)) \
                        + dist.Bernoulli(temp_3).log_prob(torch.tensor(obs, dtype=torch.float)) \
                        + temp_4
                return f_of_fair
            return f_of_k
        return f_of_obs(_)


    def p(fair):
        if fair:
            return 0.5
        else:
            return 0.2

    def halt(*args, **kwargs):
        return 0.0

    def LogSumExp(f):
        def lower(lo):
            def upper(hi):
                int_lo = int(lo)
                int_hi = int(hi)
                log_probs = torch.tensor([f(z) for z in range(int_lo, int_hi+1)])
                return torch.logsumexp(log_probs, dim=-1)
            return upper
        return lower

    if __name__ == '__main__':
        log_prob_false = coin(halt)(False)
        log_prob_true  = coin(halt)(True)
        print(log_prob_false.exp().item())
        print(log_prob_true.exp().item())
    ```
7. Execute the compiled program.
    ```
    root@f0eb7fdfbe8e /m/bench (docker)# mappl dump-pyro coin.hoisted.mappl | cat - footer.py > coin.py
    root@f0eb7fdfbe8e /m/bench (docker)# python coin.py 
    0.7400000095367432
    0.25999999046325684
    ```