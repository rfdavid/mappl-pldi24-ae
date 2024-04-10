# Step-by-step instructions on how to create new programs and compile them to Pyro

1. Enter nix shell.

    ```
    (base) ┌─[xxx@xxxx] - [~] - [Tue Apr 09, 21:34]
    └─[$] <> docker run -it mappl-pldi24-ae-ne-tested bash -c "cd /mappl-pldi24-ae/bench/new && nix --extra-experimental-features nix-command --extra-experimental-features flakes develop --command fish" 
    path '/mappl-pldi24-ae/bench/new' does not contain a 'flake.nix', searching up
    warning: Git tree '/mappl-pldi24-ae' is dirty
    Welcome to fish, the friendly interactive shell
    Type help for instructions on how to use fish
    ```

2. Create a new program.

    ```
    root@433d797c69f6 /m/b/new (newexamples)# echo -e "external def p : bool -> ureal\ndef coin(obs:bool) : Unit = {\n\tfair = sample(BERN(0.2));\n\tfactor(logPr BERN(p(fair)) at obs end)\n}\n" > coin.mappl
    root@433d797c69f6 /m/b/new (newexamples)# cat coin.mappl 
    external def p : bool -> ureal
    def coin(obs:bool) : Unit = {
        fair = sample(BERN(0.2));
        factor(logPr BERN(p(fair)) at obs end)
    }
    ```

3. Apply variable elimination.

    `var-elim` without `-hoisted` flag generates a more readable progrm.
    ```
    root@433d797c69f6 /m/b/new (newexamples)# mappl var-elim coin.mappl 
    external def p : bool -> ureal
    def coin(k : Unit -> real, obs : bool) : real =
        let k_of_fair = fun (fair : bool) =>
            logPr BERN(0.200000) at fair end +
            logPr BERN(p@(fair)) at obs end + k@(unit)
        in
        LogSumExp@(k_of_fair, false, true)
        end
    ```

    But `-hoisted` is requried for efficiency. 
    ```
    root@433d797c69f6 /m/b/new (newexamples)# mappl var-elim -hoist coin.mappl > coin.hoisted.mappl
    root@433d797c69f6 /m/b/new (newexamples)# cat coin.hoisted.mappl 
    external def p : bool -> ureal
    def coin(k : Unit -> real, obs : bool) : real =
        let k_of_fair = lambda_0@(k, obs) in
        LogSumExp@(k_of_fair, false, true)
        end

    def lambda_0(k : Unit -> real, obs : bool, fair : bool) : real =
        logPr BERN(0.200000) at fair end +
        logPr BERN(p@(fair)) at obs end + k@(unit)
    ```

4. Compile the hoisted program to Pyro.
    ```
    root@433d797c69f6 /m/b/new (newexamples)# mappl dump-pyro coin.hoisted.mappl
    import torch
    import pyro
    import pyro.distributions as dist
    from functools import cache
    import argparse

    def coin(k, obs):
        k_of_fair = cached_partial(lambda_0, k, obs)
        return LogSumExp(k_of_fair, False, True)

    @cache
    def lambda_0(k, obs, fair):
        temp_0 = p(fair)
        temp_1 = k(0.0)
        return dist.Bernoulli(0.200000).log_prob(torch.tensor(fair, dtype=torch.float)) \
            + dist.Bernoulli(temp_0).log_prob(torch.tensor(obs, dtype=torch.float)) \
            + temp_1
    ```

5. Define the external function `external def p : bool -> ureal` and other necessities (or import from [here](https://github.com/mappl-pldi24-ae/mappl-pldi24-ae/blob/newexamples/pyro/utils.py)) in a python script `footer.py`.
    ```
    root@433d797c69f6 /m/b/new (newexamples)# echo -e "def p(fair):\n    if fair:\n        return 0.5\n    else:\n        return 0.2\n\nfrom functools import partial\n@cache\ndef cached_partial(func, *args):\n    return partial(func, *args)\n    \ndef halt(*args, **kwargs):\n    return 0.0\n\ndef LogSumExp(f, lo, hi):\n    int_lo = int(lo)\n    int_hi = int(hi)\n    log_probs = torch.tensor([f(z) for z in range(int_lo, int_hi+1)])\n    return torch.logsumexp(log_probs, dim=-1)\n\nif __name__ == '__main__':\n    log_prob_false = coin(halt, False)\n    log_prob_true  = coin(halt, True)\n    print(log_prob_false.exp().item())\n    print(log_prob_true.exp().item())" | tee footer.py 
    def p(fair):
        if fair:
            return 0.5
        else:
            return 0.2

    from functools import partial
    @cache
    def cached_partial(func, *args):
        return partial(func, *args)
        
    def halt(*args, **kwargs):
        return 0.0

    def LogSumExp(f, lo, hi):
        int_lo = int(lo)
        int_hi = int(hi)
        log_probs = torch.tensor([f(z) for z in range(int_lo, int_hi+1)])
        return torch.logsumexp(log_probs, dim=-1)

    if __name__ == '__main__':
        log_prob_false = coin(halt, False)
        log_prob_true  = coin(halt, True)
        print(log_prob_false.exp().item())
        print(log_prob_true.exp().item())
    ```

6. Glue compiled program and `footer.py` together.
    ```
    root@433d797c69f6 /m/b/new (newexamples)# mappl dump-pyro coin.hoisted.mappl | cat - footer.py
    import torch
    import pyro
    import pyro.distributions as dist
    from functools import cache
    import argparse

    def coin(k, obs):
        k_of_fair = cached_partial(lambda_0, k, obs)
        return LogSumExp(k_of_fair, False, True)

    @cache
    def lambda_0(k, obs, fair):
        temp_0 = p(fair)
        temp_1 = k(0.0)
        return dist.Bernoulli(0.200000).log_prob(torch.tensor(fair, dtype=torch.float)) \
            + dist.Bernoulli(temp_0).log_prob(torch.tensor(obs, dtype=torch.float)) \
            + temp_1

    def p(fair):
        if fair:
            return 0.5
        else:
            return 0.2

    from functools import partial
    @cache
    def cached_partial(func, *args):
        return partial(func, *args)
        
    def halt(*args, **kwargs):
        return 0.0

    def LogSumExp(f, lo, hi):
        int_lo = int(lo)
        int_hi = int(hi)
        log_probs = torch.tensor([f(z) for z in range(int_lo, int_hi+1)])
        return torch.logsumexp(log_probs, dim=-1)

    if __name__ == '__main__':
        log_prob_false = coin(halt, False)
        log_prob_true  = coin(halt, True)
        print(log_prob_false.exp().item())
        print(log_prob_true.exp().item())
    ```
7. Execute the compiled program.
    ```
    root@433d797c69f6 /m/b/new (newexamples)# mappl dump-pyro coin.hoisted.mappl | cat - footer.py > coin.py
    root@433d797c69f6 /m/b/new (newexamples)# python coin.py 
    0.7400000095367432
    0.25999999046325684
    ```

Step 3-4 can be done in one command `mappl pyro coin.mappl`, and Step 5-7 can be easily automated with scripts. But we don’t do it here for illustration purposes.
