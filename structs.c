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
struct point *ptadd(struct point *p1, struct point *p2);
/*
char *printpt(struct point p){

    printf("[");
    mpz_out_str(stdout, 10, p.i);
    printf(",");
    mpz_out_str(stdout, 10, p.j);
    printf("]");
    printf("\n");
}*/
struct point *ptmul( mpz_t n, struct point *p1)
{
    /*struct poly *curve = (struct poly *)malloc(sizeof(struct poly));
    struct point *sum  = (struct point *)malloc(sizeof(struct point));
 
    mpz_t xcoeff;
    mpz_init_set(xcoeff, p1->curve->x_coeff);

    mpz_t c;
    mpz_init_set(c, p1->curve->c);
 
    mpz_t mod;
    mpz_init_set(mod, p1->curve->mod);
 
    Poly( curve, xcoeff, c, mod);*/

    struct point *product = p1;
    //copy n into new mpz_t
    mpz_t i;
    mpz_init(i);
    mpz_set(i, n);
    mpz_sub_ui( i, i, (unsigned long) 1);
    while( mpz_sgn(i) != 0)
    {
        product = ptadd(product, p1);
        mpz_sub_ui( i, i, (unsigned long) 1);
    }
    return product;
}
/*
struct point *ptadd(struct point *p1, struct point *p2){
    struct poly *curve = (struct poly *)malloc(sizeof(struct poly));
    struct point *sum  = (struct point *)malloc(sizeof(struct point));

    mpz_t xcoeff;
    mpz_init_set(xcoeff, p1->curve->x_coeff);

    mpz_t c;
    mpz_init_set(c, p1->curve->c);

    mpz_t mod;
    mpz_init_set(mod, p1->curve->mod);

    Poly( curve, xcoeff, c, mod);
    return ptaddhelper( sum, p1, p2);
}*/
int pteq(struct point *p1, struct point *p2)
{
    if(mpz_cmp(p1->i, p2->i) == 0 &&
            mpz_cmp(p1->j, p2->j) == 0 &&
            mpz_cmp(p1->curve->mod, p2->curve->mod) == 0 &&
            mpz_cmp(p1->curve->c, p2->curve->c) == 0 &&
            mpz_cmp(p1->curve->x_coeff, p2->curve->x_coeff) == 0)
    {
        return 1;
    }
    else
    {
        return 0;
    }
}

int ptneq(struct point *p1, struct point *p2)
{
    if(mpz_cmp(p1->i, p2->i) != 0 ||
            mpz_cmp(p1->j, p2->j) != 0 ||
            mpz_cmp(p1->curve->mod, p2->curve->mod) != 0 ||
            mpz_cmp(p1->curve->c, p2->curve->c) != 0 ||
            mpz_cmp(p1->curve->x_coeff, p2->curve->x_coeff) != 0)
    {         
        return 1;
    }
    else
    {

         return 0;
    }
}

struct point *ptneg(struct point *p1)
{
    struct poly *curve = (struct poly *)malloc(sizeof(struct poly));
    struct point *neg  = (struct point *)malloc(sizeof(struct point));

    mpz_t xcoeff;
    mpz_init_set(xcoeff, p1->curve->x_coeff);

    mpz_t c;
    mpz_init_set(c, p1->curve->c);

    mpz_t mod;
    mpz_init_set(mod, p1->curve->mod);

    Poly( curve, xcoeff, c, mod);
    if( mpz_sgn(p1->i) == -1 && mpz_sgn(p1->j) == -1 ){
        Point( neg, p1->i, p1->j, curve);
    }
    else{
        mpz_t inv;
        mpz_init(inv);
        mpz_neg(inv, p1->j);
        mpz_mod(inv, inv, mod);

        Point( neg, p1->i, inv, curve );

        mpz_clear(inv);
    }

    mpz_clear(xcoeff);
    mpz_clear(c);
    mpz_clear(mod);

    return neg;
}

struct point *ptadd(struct point *p1, struct point *p2)
{
    struct poly *curve = (struct poly *)malloc(sizeof(struct poly));
    struct point *sum  = (struct point *)malloc(sizeof(struct point));

    mpz_t xcoeff;
    mpz_init_set(xcoeff, p1->curve->x_coeff);

    mpz_t c;
    mpz_init_set(c, p1->curve->c);

    mpz_t mod;
    mpz_init_set(mod, p1->curve->mod);

    Poly( curve, xcoeff, c, mod);

    mpz_t zero;
    mpz_init_set_str(zero, "0", 10);

    mpz_t p3x;
    mpz_t p3y;

    mpz_init(p3x);
    mpz_init(p3y);

    /* if pt is -1, -1 -> pt at infinity acts as identity element
     * return other point
     */

    if( mpz_sgn(p1->i) == -1 && mpz_sgn(p1->j) == -1 ){
        mpz_set(p3x, p2->i);
        mpz_set(p3y, p2->j);
        Point( sum, p3x, p3y, curve);

        
    } else if ( mpz_sgn(p2->i) == -1 && mpz_sgn(p2->j) == -1 ){
        mpz_set(p3x, p1->i);
        mpz_set(p3y, p1->j);
        Point( sum, p3x, p3y, curve);

    }
    else{
    /* build local x and y coords */
        
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
        mpz_mod(p2y, p2->j, mod);

        /* check if they are inverses of one another */

        mpz_t neg;
        mpz_init(neg);
        mpz_neg(neg, p2y);
        if(mpz_congruent_p(p1y, neg, mod))
        {
            mpz_set_str(p3x, "-1", 10);
            mpz_set_str(p3y, "-1", 10);
            Point( sum, p3x, p3y, curve);

        /*mpz_clear(neg);
        mpz_clear(p1x);
        mpz_clear(p1y);
        mpz_clear(p2x);
        mpz_clear(p2y);*/

        /*mpz_clear(xcoeff);
        mpz_clear(c);
        mpz_clear(z:qero);
        mpz_clear(mod);*/

           // return sum;
        }
        else{
            
            //slope

            mpz_t m;
            mpz_init(m);
    
            /* if pts are not the same */
            if(mpz_cmp(p1x, p2x) != 0 || mpz_cmp(p1y, p2y) != 0)
            {
                mpz_t tmpy;
                mpz_t tmpx;
                mpz_init(tmpy);
                mpz_init(tmpx);

                mpz_sub(tmpy, p2y, p1y);
                mpz_sub(tmpy, tmpy, mod);
                mpz_sub(tmpx, p2x, p1x);
                mpz_mod(tmpx, tmpx, mod);

                mpz_invert(tmpx, tmpx, mod);
                mpz_mul(m, tmpy, tmpx);
                mpz_mod(m, m, mod);
        
                mpz_clear(tmpy);
                mpz_clear(tmpx);
            } else { /* if points are same */
                mpz_t tmpx;
                mpz_t tmpy;
                mpz_init(tmpx);
                mpz_init(tmpy);

                mpz_mul(tmpx, p1x, p1x);
                mpz_mod(tmpx, tmpx, mod);
                mpz_mul_si(tmpx, tmpx, (long) 3);
                mpz_mod(tmpx, tmpx, mod);
                mpz_add(tmpx, tmpx, xcoeff);
                mpz_mul_si(tmpy, p1y, (long) 2);
                mpz_mod(tmpy, tmpy, mod);
                mpz_invert(tmpy, tmpy, mod);
                mpz_mul(m, tmpx, tmpy);
                mpz_mod(m, m, mod);

                mpz_clear(tmpx);
                mpz_clear(tmpy);
            }

            /* find p3x */
            mpz_t tmp;
            mpz_init(tmp);
            mpz_mul(tmp, m, m);
            mpz_mod(tmp, tmp, mod);
            mpz_sub(tmp, tmp, p1x);
            mpz_sub(tmp, tmp, p2x);
            mpz_mod(tmp, tmp, mod);
            mpz_set(p3x, tmp);

            /* find p3y */

            mpz_sub(tmp, p1x, p3x);
            mpz_mul(tmp, tmp, m);
            mpz_sub(tmp, tmp, p1y);
            mpz_mod(tmp, tmp, mod);
            mpz_set(p3y, tmp);

            /* build pt */

            Point( sum, p3x, p3y, curve );

            mpz_clear(m);        
            mpz_clear(tmp);
        }

        mpz_clear(neg);

        mpz_clear(p1x);
        mpz_clear(p1y);
        mpz_clear(p2x);
        mpz_clear(p2y);
    }

    mpz_clear(xcoeff);
    mpz_clear(c);
    mpz_clear(zero);
    mpz_clear(mod);


    mpz_clear(p3x);
    mpz_clear(p3y);


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

