#!/bin/bash
set -x
./prime.native $1.pr > $1.ll
llc -relocation-model=pic $1.ll > $1.s
cc -c gmpfunc.c
cc -c structs.c
cc -c input.c
gcc -o $1.exe $1.s gmpfunc.o structs.o input.o -lgmp
./$1.exe
rm $1.ll;
rm $1.s;
rm $1.exe;
rm *.o
