// This file will be used to interface with OCaml LLVM
#include <stdio.h>
#include <gmp.h>
#include <stdlib.h>
#include <string.h>
#include "structs.h"


void printl(mpz_t n)
{
    mpz_out_str(stdout, 10, n);
    printf("\n");
}

int rand_func(mpz_t rand, mpz_t seed, mpz_t max)
{
    gmp_randstate_t state; /*intialize state */

    gmp_randinit_mt(state); /* set set state to use the Mersenne Twister Algorithm */
    gmp_randseed(state, seed); /*seed the state using user input*/

    mpz_urandomm(random, state, max); /*generate random int*/

    gmp_randclear(state);
    return(0);
}

char *sub(char *left, char *right)
{
    mpz_t n1;
    mpz_t n2;
    mpz_init(n1);
    mpz_init(n2);
    if (mpz_set_str(n1, left, 10) != 0){
        printf("Failed to assign number");
        mpz_clear(n1);
        mpz_clear(n2);
        exit(1);
    }
    if (mpz_set_str(n2, right, 10) != 0) {
        printf("Failed to assign number");
        mpz_clear(n1);
        mpz_clear(n2);
        exit(1);
    }
    mpz_sub(n1, n1, n2);
    char *ret_str = mpz_get_str(NULL, 10, n1);
    mpz_clear(n1);
    mpz_clear(n2);
    return ret_str;
}

int eq_func(mpz_t x, mpz_t y){
    if(mpz_cmp(x, y) == 0){
        return 1;
    }
    else{
        return 0;
    }
}

int neq_func(mpz_t x, mpz_t y){
    if(mpz_cmp(x, y) == 0){
        return 0;
    }
    else{
        return 1;
    }
}

int lth_func(mpz_t x, mpz_t y){
    if(mpz_cmp(x, y) < 0){
        return 1;
    }
    else{
        return 0;
    }
}

int gth_func(mpz_t x, mpz_t y){
    if(mpz_cmp(x, y) > 0){
        return 1;
    }
    else{
        return 0;
    }
}

int leq_func(mpz_t x, mpz_t y){
    if(mpz_cmp(x, y) <= 0){
        return 1;
    }
    else{
        return 0;
    }
}

int geq_func(mpz_t x, mpz_t y){
    if(mpz_cmp(x, y) >= 0){
        return 1;
    }
    else{
        return 0;
    }
}

#ifdef BUILD_TEST
int main()
{

    // Create a lint through assignment to an id
    // char *id1 = "1934759237458927349587234858395728";
    // printf("n = ");
    // printl(id1);
    // printf("\n");

    // // Do some operation(s) on lint
    // printf("Squaring:\n");
    // char *fun = pow(id1, 2);
    // printf("%s", fun);
    // printf("\n");


    // printf("Adding\n");
    // char *added = add("4035273409750284735027430528934750", "139487619823469187364916398427");
    // printf("%s", added);
    // printf("\n");

    // // clean up
    // free(fun);
    // free(added);
    return 0;
}
#endif
