#!/bin/bash
./prime.native $1.pr > $1.ll
llc -relocation-model=pic $1.ll > $1.s
gcc -o $1.exe $1.s
./$1.exe
rm $1.ll;
rm $1.s;
rm $1.exe;
