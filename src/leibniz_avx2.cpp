#include <cstdio>

#include <immintrin.h>


double _x = 1.0;
double pi = 1.0;


int main()
{
    unsigned rounds;
    unsigned int unroll = 4;
    auto infile = std::fopen("rounds.txt", "r");  // open file
    std::fscanf(infile, "%u", &rounds);           // read from file
    std::fclose(infile);                          // close file
    
    __m256d x       = _mm256_set_pd(-1.0,1.0,-1.0,1.0);
    __m256d den     = _mm256_set_pd(0.0,0.0,0.0,0.0);
    __m256d inc     = _mm256_set_pd(4.0,4.0,4.0,4.0);
    __m256d two     = _mm256_set_pd(2.0,2.0,2.0,2.0);
    __m256d mone    = _mm256_set_pd(-1.0,-1.0,-1.0,-1.0);
    __m256d one     = _mm256_set_pd(1.0,1.0,1.0,1.0);
    __m256d ivec    = _mm256_set_pd(2.0,3.0,4.0,5.0);
    __m256d pivec   = _mm256_set_pd(0.0,0.0,0.0,0.0);

    rounds += 2u; // do this outside the loop
    unsigned int vec_end = rounds - rounds % unroll;

    for (unsigned i=2u ; i < vec_end ; i+=unroll) // use ++i instead of i++
    {
        //#x = -x; // some compilers optimize this better than x *= -1
        // compute den = (2 * i - 1)
        den     = _mm256_add_pd(_mm256_mul_pd(two,ivec),mone);

        // increment ivec, so ivec += inc
        ivec    = _mm256_add_pd(ivec,inc);

        // compute partial sums
        pivec   = _mm256_add_pd(pivec,_mm256_div_pd(x,den));
    }

    // gather the partial sums
    double* pi_v = (double*)&pivec;
    pi +=  pi_v[0] + pi_v[1] + pi_v[2] + pi_v[3];

    // now the wind-down loop
    for (unsigned i=vec_end ; i < rounds ; ++i) 
    {
        _x = -_x;
        pi += (_x / (2u * i - 1u));
    }

    pi *= 4;
    std::printf("%.16f\n", pi); // print 16 decimal digits of pi
}
