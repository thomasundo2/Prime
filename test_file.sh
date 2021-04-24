#!/bin/bash
./prime.native $1.pr > $1.ll
llc -relocation-model=pic $1.ll > $1.s
cc -c gmpfunc.c
cc -c structs.c
gcc -o $1.exe $1.s gmpfunc.o structs.o -lgmp
./$1.exe
rm $1.ll;
rm $1.s;
rm $1.exe;
