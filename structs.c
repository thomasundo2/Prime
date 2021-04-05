// This file will be used to interface with OCaml LLVM
#include <stdio.h>
#include <gmp.h>
#include <stdlib.h>
#include <string.h>

// TODO: move to its own file
#include"structs.h"
struct point Point(int i, int j)
{
		struct point p;
		p.i = i;
		p.j = j;
		return p;
}
char *printpt(struct point p){
    char i_str[20];
    char j_str[20];

    sprintf(i_str, "[%d,", p.i);
    sprintf(j_str, "%d]", p.j);
    char *ret = strcat(i_str, j_str);
    return ret;
}

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
}
