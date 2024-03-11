import pyro
import pyro.distributions as dist
import torch
from functools import *
import pyro.contrib as contrib
from pyro.infer.importance import Importance
from time import time

import sys
sys.setrecursionlimit(32767)

TensorIntBool = torch.tensor([0, 1], dtype=torch.int)
max_tries=2**256

def LogSumExp(f):
    def lower(lo):
        def upper(hi):
            int_lo = int(lo)
            int_hi = int(hi)
            log_probs = torch.tensor([f(z) for z in range(int_lo, int_hi+1)])
            return torch.logsumexp(log_probs, dim=-1)
        return upper
    return lower
    
def target(f, *args, num_samples=None):
    if num_samples is None:
        with pyro.poutine.block():
            with pyro.poutine.trace() as t:
                f(*args)
        return t.trace.log_prob_sum()
    else:
        importance = Importance(f, guide=None, num_samples=num_samples)
        posterior = importance.run(*args)
        log_probs = posterior.log_weights
        me = torch.logsumexp(torch.tensor(log_probs), dim=-1) - torch.log(torch.tensor(num_samples))
        return me

def timing(f):
    @wraps(f)
    def wrap(*args, **kw):
        ts = time()
        result = f(*args, **kw)
        te = time()
        tdiff = te-ts
        # print('func:%r args:[%r, %r] \t took: %2.4f sec\treturns [%r] ' % (f.__name__, args, kw, tdiff, result))
        return {"return": result, "time": tdiff}
    return wrap

@timing
def model_evidence(method, f, *args, verbose=False, num_samples=None):
    if method == "enumerate":
        # print(max_tries)
        enum = contrib.oed.search.Search(f, max_tries=max_tries)
        enum.run(*args)
        model_evidence = torch.logsumexp(torch.tensor(enum.log_weights), dim=-1)
        if verbose:
            for t in enum.exec_traces:
                vals = {
                    name:(t.nodes[name]["value"], torch.exp(t.nodes[name]["log_prob_sum"]).item())
                    for name in t.nodes
                    if "value" in t.nodes[name]
                    and "log_prob_sum" in t.nodes[name]
                }
                print(vals)
    elif method == "transform":
        model_evidence = f(*args)
    elif method =="importance":
        importance = Importance(f, guide=None, num_samples=num_samples)
        posterior = importance.run(*args)
        log_probs = posterior.log_weights
        model_evidence = torch.logsumexp(torch.tensor(log_probs), dim=-1) - torch.log(torch.tensor(num_samples))
    else:
        assert False
    return model_evidence
# @cache
# https://github.com/python/cpython/blob/326f0ba1c5dda1d9613dbba11ea2470654b0d9c8/Lib/functools.py#L652

def tensor_to_tuple(tensor):
    if tensor.dim() == 0:
        return tensor.item()
    elif tensor.dim() == 1:
        return tuple(tensor.tolist())
    else:
        return tuple(tensor_to_tuple(t) for t in tensor)

def tuple_to_tensor(tuple,**kwargs):
    return torch.tensor(tuple, **kwargs)

def float_tensor(d):
    return torch.tensor(d, dtype=torch.float)

def halt(*args):
    return None

def halt_transformed(*args):
    return torch.zeros(1)
