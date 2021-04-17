// This file will be used to interface with OCaml LLVM
#include <stdio.h>
#include <gmp.h>
#include <stdlib.h>

int main(void)
{
    mpz_t n;
    // int flag;
    // mpz_init(n);
    // flag = mpz_set_str(n, "2223423423423432", 10);
    mpz_init_set_str(n, "247586274356435345", 10);
    mpz_out_str(stdout, 10, n);
    mpz_add(n, n, n);
    mpz_clear(n);
    return 0;
}