#include <stdio.h>
#include <gmp.h>
#include <string.h>
#include <stdlib.h>

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

char *decode(mpz_t in)
{
    int i;
    char *lintStr = mpz_get_str(NULL, 10, in);
    int padLen = strlen(lintStr) % 3;
    char *tmp = (char *) malloc(strlen(lintStr)+padLen+1);
    for (i = 0; i < padLen; i++)
        tmp[i] = '0';
    tmp[i] = '\0';
    strncat(tmp, lintStr, strlen(lintStr));
    free(lintStr);
    return tmp;
}

#ifdef BUILD_TEST
int main()
{
    mpz_t res;
    // mpz_init(res);
    char testStr[] = "HelloWorld";
    encode(res, testStr);
    mpz_out_str(stdout, 10, res);
    printf("\n");
    printf("%s\n", decode(res));
}
#endif
