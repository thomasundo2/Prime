// This file will be used to interface with OCaml LLVM
#include <stdio.h>
#include <gmp.h>
#include <stdlib.h>
#include <string.h>

// TODO: move to its own file
#include"structs.h"
struct point Point(mpz_t i, mpz_t j)
{
		// printf("testing!\n");
		struct point p;
		mpz_init_set(p.i, i);
		mpz_init_set(p.j, j);
		return p;
}

void printpt(struct point *p)
{
    printf("[");
    mpz_out_str(stdout, 10, p->i);
    printf(",");
    mpz_out_str(stdout, 10, p->j);
    printf("]\n");
}

/*
char *printpt(struct point p){

    printf("[");
    mpz_out_str(stdout, 10, p.i);
    printf(",");
    mpz_out_str(stdout, 10, p.j);
    printf("]");
    printf("\n");
}*/

/*
struct point ptadd(struct point p1, struct point p2)
{

    int i, j;
    int m;
    int b = 1;

    if(p1.i == p2.i && p1.j == p2.j){
	m = (3*(p1.i)^2 + b)/(2*p1.j);
    }
    else{
	m = (p2.j-p1.j)/(p2.i-p1.i);
    }

    i = m^2 - p1.i - p2.i;
    j = m*(p1.i - i) - p1.j;

    return Point(i, j);
}*/

