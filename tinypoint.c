#include <stdio.h>
#include <gmp.h>
#include <stdlib.h>

struct point {
    mpz_t i;
    mpz_t j;
};

void Point(struct point *p, mpz_t left, mpz_t right)
{
    // printf("Testin!\n");
    mpz_init_set(p->i, left);
    mpz_init_set(p->j, right);
}

int main() {
    struct point p;
    struct point p2;
    mpz_t n1, n2;
    mpz_init_set_str(n1, "1", 10);
    mpz_init_set_str(n2, "2", 10);
    Point(&p, n1, n2);
    p2 = p;
    mpz_out_str(stdout, 10, p.i);
    mpz_out_str(stdout, 10, p.j);
    return 0;
}