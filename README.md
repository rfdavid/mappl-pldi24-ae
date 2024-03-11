# [**PLDI'24 Artifact**] Variable Elimination for an Expressive Probabilistic Programming Language

This repository contains the tool source code, benchmarks and instructions to reproduce the results in the paper. 

## Table of Contents

* [Repo overview](#overview)
* [Platform requirements](#platform-requirements)
* [Getting started](#getting-started)
* [Experimental Evaluation](#experimental-evaluation)

## Overview

```console
$ mappl
mappl compiler
  mappl SUBCOMMAND
=== subcommands ===
  dump-pyro                  . perform ANF conversion into pyro code generation
  dump-rust                  . rust code generation
  hoist                      . lambda hoisting
  only-parse                 . only parse
  type-check                 . type check
  var-elim                   . variable elimination
  version                    . print version information
  help                       . explain a given subcommand (perhaps recursively)


$ mappl var-elim --help
variable elimination
  mappl var-elim FILENAME
=== flags ===
  [-hoist]                   . apply lambda hoisting when it is on
  [-output _]                . output file
  [-time]                    . count running time when it is on
  [-verbose]                 . verbose logging for debug
  [-help], -?                . print this help text and exit

$ mappl hoist --help 
lambda hoisting
  mappl hoist FILENAME
=== flags ===
  [-output _]                . output file
  [-help], -?                . print this help text and exit

$ mappl dump-pyro  --help
perform ANF conversion into pyro code generation
  mappl dump-pyro FILENAME
=== flags ===
  [-output _]                . output file
  [-help], -?                . print this help text and exit
```

## Platform requirements

To set up the environment with all required dependencies, you will build the environment from scratch using [Nix](https://nixos.org/download). We uses Nix version 2.20.5. All experiments were run on a workstation with a 3.6GHz Intel Core i7-7820X processor and 12GB of memory. This Nix environment is not for Apple ARM processors.

## Getting Started

### Set up the environment

#### Install Nix

```
sh <(curl -L https://nixos.org/nix/install) --daemon
```

Please refer to https://nixos.org/download for more info.

#### Using nix

To set up the dependencies for running artifact evaluation, run:
```
nix --experimental-features 'nix-command flakes' develop -i
```
Verify the nix shell is running by typing `which mappl`. The output should be similar to:

```
> /nix/store/aqqh0r7lbgl3zwz9ckygd2l5pq3x8y9g-ocaml4.14.1-mappl-1.0/bin/mappl
``` 

## Experimental Evaluation

Benchmark scripts to reproduce results from the paper are at `bench/PLDI`.
