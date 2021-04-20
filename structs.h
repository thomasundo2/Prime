#ifndef STRUCTS_H
#define STRUCTS_H


struct poly {
    mpz_t x_coeff;
    mpz_t c;
    mpz_t mod;
};

struct point
{
	mpz_t i;
	mpz_t j;
	struct poly *curve;
};

#endif
