int main(){
    mpz_t test; //lint test;
    mpz_init(test);
    //every time run across storage and run contructor mpz_init()
    
    mpz_set_str(test, "123412341341231324132132412413241234123", 10); //test = "123412341341231324132132412413241234123";
    mpz_out_str(stdout, 10, test); //printl(test);
    {
    mpz_t tmp_sum; 
    mpz_init(tmp_sum);
    mpz_add(tmp_sum, test, test); //test + test;
    mpz_out_str(sstdout, 10, tmp_sum); //printl(test + test);
    mpz_clear(tmp_sum);
    }
    
    mpz_clear(test);

    return 0;
}
