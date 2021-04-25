#include <stdio.h>
#include <gmp.h>
#include <string.h>

// takes in pointer to mpz to update
void encode(mpz_t res, char *in)
{
    // keep output buff that handles padding length and null terminator
    // printf("%s\n", in);
    char outBuf[3 * strlen(in) + 1];
    int i;
    outBuf[0] = '\0';
    for (i = 0; i < strlen(in); i++) {
        int c = (int) in[i];
        char temp[4];
        sprintf(temp, "%03d", c);
        strncat(outBuf, temp, strlen(temp));
        // printf("%s\n", outBuf);
    }
    mpz_init_set_str(res, outBuf, 10);
}

#ifdef BUILD_TEST
int main()
{
    mpz_t res;
    // mpz_init(res);
    char testStr[] = "HelloWorld";
    encode(res, testStr);
    mpz_out_str(stdout, 10, res);
}
#endif
