// This file will be used to interface with OCaml LLVM
#include <stdio.h>
#include <gmp.h>
#include <stdlib.h>
#include <string.h>

// Don't need assign here. Keep as string until necessary


// void printl(char *num)
// {
//     mpz_t n;
//     mpz_init(n);
//     mpz_set_ui(n, 0);
//     if (mpz_set_str(n, num, 10) != 0) {
//         printf("Failed to assign number");
//         mpz_clear(n);
//         exit(1);
//     }
//     mpz_out_str(stdout, 10, n);
//     mpz_clear(n);
// }

void init(mpz_t *n)
{
    return;
}

void printl(mpz_t n)
{
    mpz_out_str(stdout, 10, n);
    printf("\n");
}

// raise to power. returns string so that we can store in symbol table
// Q: How to return without leaking memory
char *powerm(char *num, char *power, char *mod)
{
    mpz_t n1;
    mpz_t n2;
    mpz_t n3;
    mpz_init(n1);
    mpz_init(n2);
    if (mpz_set_str(n1, num, 10) != 0) {
        printf("Failed to assign number");
        exit(1);
    }
    if (mpz_set_str(n2, power, 10) != 0) {
        printf("Failed to assign number");
        exit(1);
    }
    mpz_init(n3);
    char *modulo = mod;
    if (modulo == NULL) {
        modulo = "10";
    }
    if (mpz_set_str(n3, modulo, 10) != 0) {
        printf("Failed to assign number");
        exit(1);
    }
    mpz_powm(n1, n1, n2, n3);
    mpz_clear(n3);
    mpz_out_str(stdout, 10, n1);
    mpz_clear(n1);
    mpz_clear(n2);
    return NULL;
}

// for lint exponentiation by an integer power
char *power(char *base, unsigned long int exp)
{
    mpz_t n1;
    mpz_init(n1);
    if (mpz_set_str(n1, base, 10) != 0) {
        printf("Failed to assign number");
        return NULL;
    }
    mpz_pow_ui(n1, n1, exp);
    char *ret_str = mpz_get_str(NULL, 10, n1);
    mpz_clear(n1);
    return ret_str;
}


// for lint addition
char *add(char *left, char *right)
{
    mpz_t n1;
    mpz_t n2;
    mpz_init(n1);
    mpz_init(n2);
    if (mpz_set_str(n1, left, 10) != 0) {
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
    mpz_add(n1, n1, n2);
    // mpz_out_str(stdout, 10, n1);
    char *ret_str = mpz_get_str(NULL, 10, n1);
    mpz_clear(n1);
    mpz_clear(n2);
    return ret_str;
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
