#ifndef STRUCTS_H
#define STRUCTS_H
struct point
{
	mpz_t i;
	mpz_t j;
};

struct poly {
    mpz_t x_coeff;
    mpz_t c;
    mpz_t mod;
};

#endif
