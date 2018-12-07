#!/bin/bash
FILE=$1

if [[ "$FILE" == *.hs ]]; then
    FILE=${FILE::-3}
fi

cd $PWD
ghc $FILE
./$FILE < input.txt
rm $FILE.hi $FILE.o $FILE
