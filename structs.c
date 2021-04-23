// This file will be used to interface with OCaml LLVM
#include <stdio.h>
#include <gmp.h>
#include <stdlib.h>
#include <string.h>

#include"structs.h"
// struct point Point(mpz_t i, mpz_t j)
// {
//     // printf("testing!\n");
//     struct point p;
//     mpz_init_set(p.i, i);
//     mpz_init_set(p.j, j);
//     return p;
// }



// POLYS
void Poly(struct poly *p, mpz_t x_coeff, mpz_t c, mpz_t mod)
{
    mpz_init_set(p->x_coeff, x_coeff);
    mpz_init_set(p->c, c);
    mpz_init_set(p->mod, mod);
}
void printpoly(struct poly *p)
{
    printf("[(");
    mpz_out_str(stdout, 10, p->x_coeff);
    printf(",");
    mpz_out_str(stdout, 10, p->c);
    printf(") : ");
    mpz_out_str(stdout, 10, p->mod);
    printf("]\n");
}


void Point(struct point *p, mpz_t i, mpz_t j, struct poly *curve)
{
    mpz_init_set(p->i, i);
    mpz_init_set(p->j, j);
    p->curve = curve;
}

void printpt(struct point *p)
{
    printf("[");
    mpz_out_str(stdout, 10, p->i);
    printf(",");
    mpz_out_str(stdout, 10, p->j);
    printf("] & ");
    printpoly(p->curve);
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


struct point *ptadd(struct point *p1, struct point *p2)
{
    struct poly *curve;
    struct point *sum;

    mpz_t xcoeff;
    mpz_init_set(xcoeff, p1->curve->x_coeff);
    printf("%d\n", 1);

    mpz_t c;
    mpz_init_set(c, p1->curve->c);
    printf("%d\n", 1);

    mpz_t mod;
    mpz_init_set(mod, p1->curve->mod);
    printf("%d\n", 1);

    Poly( curve, xcoeff, c, mod);

    printf("%d\n", 1);

    mpz_t zero;
    mpz_init_set_str(zero, "0", 10);

    mpz_t p3x;
    mpz_t p3y;

    mpz_init(p3x);
    mpz_init(p3y);

    /* if pt is -1, -1 -> pt at infinity acts as identity element
     * return other point
     */

    if( mpz_sgn(p1->i) == -1 && mpz_sgn( p1->j) == -1 ){
        mpz_set(p3x, p2->i);
        mpz_set(p3y, p2->j);
        Point( sum, p3x, p3y, curve);

        /*mpz_clear(xcoeff);
        mpz_clear(c);
        mpz_clear(zero);
        mpz_clear(mod);*/

        return sum;
    } else if ( mpz_sgn(p2->i) == -1 && mpz_sgn(p2->j) == -1 ){
        mpz_set(p3x, p1->i);
        mpz_set(p3y, p1->j);
        Point( sum, p3x, p3y, curve);

        /*mpz_clear(xcoeff);
        mpz_clear(c);
        mpz_clear(zero);
        mpz_clear(mod);*/

        return sum;
    }

    /* build local x and y coords */
    /*

    mpz_t p1x;
    mpz_t p1y;
    mpz_t p2x;
    mpz_t p2y;

    mpz_init(p1x);
    mpz_init(p1y);
    mpz_init(p2x);
    mpz_init(p2y);

    mpz_mod(p1x, p1->i, mod);
    mpz_mod(p1y, p1->j, mod);
    mpz_mod(p2x, p2->i, mod);
    mpz_mod(p2y, p2->j, mod);*/

    /* check if they are inverses of one another */

    /*mpz_t neg;
    mpz_init(neg);
    mpz_neg(neg, p2y);
    if(mpz_congruent_p(p1y, neg, mod))
    {
        mpz_set_str(p3x, "-1", 10);
        mpz_set_str(p3y, "-1", 10);
        Point( sum, p3x, p3y, curve);

        mpz_clear(neg);
        mpz_clear(p1x);
        mpz_clear(p1y);
        mpz_clear(p2x);
        mpz_clear(p2y);

        mpz_clear(xcoeff);
        mpz_clear(c);
        mpz_clear(zero);
        mpz_clear(mod);

        return sum;
    }*/
    

    //slope

    /* mpz_t m;
    mpz_init(m);

    if(mpz_cmp(p1x, p2x) == 0 && mpz_cmp(p1y, p2y))
    {
        mpz_t tmpy;
        mpz_t tmpx;
        mpz_t tmpxinv;
        mpz_t tmp_m
        

    }*/

    printf("%d\n", 1);
    Point( sum, zero, zero, curve );

    mpz_clear(xcoeff);
    mpz_clear(c);
    mpz_clear(zero);
    mpz_clear(mod);


    return sum;
    /*int i, j;
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

    return Point(i, j);*/
}

