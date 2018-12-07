# Advent of Code

This Repository is my implementations of the [Advent of Code](https://adventofcode.com/)

I will be implementing things in Haskell.


# Usage

Each file is meant to be run individually.

First compile it with ghc. Then call it and redirect the input file into it.

```console
$ ghc dayXX-X.hs
$ ./dayXX-X < input.txt
```

There is also shell script to handle this for you. Just call [compileAndRun.sh](compileAndRun.sh)
and pass it the name of the challenge to build and run.

```console
$ ../../compileAndRun.sh dayXX-X
```
