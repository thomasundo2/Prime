// This file will be used to interface with OCaml LLVM
#include <stdio.h>
#include <gmp.h>
#include <stdlib.h>
#include <string.h>

// Don't need assign here. Keep as string until necessary

void printl(char *num)
{
    mpz_t n;
    mpz_init(n);
    mpz_set_ui(n, 0);
    if (mpz_set_str(n, num, 10) != 0) {
        printf("Failed to assign number");
        exit(1);
    }
    mpz_out_str(stdout, 10, n);
    mpz_clear(n);
}


// raise to power. returns string so that we can store in symbol table
// TODO: How to return without leaking memory
char *power(char *num, char *power, char *mod)
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

char *add(char *left, char *right)
{
    mpz_t n1;
    mpz_t n2;
    mpz_init(n1);
    mpz_init(n2);
    if (mpz_set_str(n1, left, 10) != 0) {
        printf("Failed to assign number");
        exit(1);
    }
    if (mpz_set_str(n2, right, 10) != 0) {
        printf("Failed to assign number");
        exit(1);
    }
    mpz_add(n1, n1, n2);
    // mpz_out_str(stdout, 10, n1);
    char *ret_str = mpz_get_str(NULL, 10, n1);
    mpz_clear(n1);
    mpz_clear(n2);
    return ret_str;
}

#ifdef BUILD_TEST
int main()
{
    // Create a lint through assignment to an id
    char *id1 = "1934759237458927349587234858395728";
    printf("n = ");
    printl(id1);
    printf("\n");
    // Do some operation(s) on lint
    printf("Squaring\n");
    char *fun = power(id1, "23809237450243750274503745", "10");
    printf("\n");

    printf("Adding\n");
    char *added = add("4035273409750284735027430528934750", "139487619823469187364916398427");
    
    printf("%s", added);
    printf("\n");
    // clean up
    return 0;
}
#endif