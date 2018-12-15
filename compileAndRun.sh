#!/bin/bash
FILE=$1
INPUT=$2

[ -z "$FILE" ] && exit 1
[ -z "$INPUT" ] && INPUT="input.txt"

[[ "$FILE" == *.hs ]] && FILE=${FILE::-3}

cd $PWD
echo "Compiling $FILE"
ghc $FILE

echo "Running $FILE with input $INPUT"
time ./$FILE < $INPUT
rm $FILE.hi $FILE.o $FILE
