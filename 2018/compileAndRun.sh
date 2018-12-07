#!/bin/sh
cd $PWD
ghc $1
./$1 < input.txt
rm $1.hi $1.o $1
