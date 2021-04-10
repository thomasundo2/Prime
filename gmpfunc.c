// This file will be used to interface with OCaml LLVM
#include <stdio.h>
#include <gmp.h>
#include <stdlib.h>
#include <string.h>
#include"structs.h"


void printl(mpz_t n)
{
    mpz_out_str(stdout, 10, n);
    printf("\n");
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
