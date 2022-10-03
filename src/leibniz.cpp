#include <cstdio>

unsigned rounds;
double x = 1.0;
double pi = 1.0;

int main()
{
    auto infile = std::fopen("rounds.txt", "r");  // open file
    std::fscanf(infile, "%u", &rounds);           // read from file
    std::fclose(infile);                          // close file
    
    rounds += 2u; // do this outside the loop
    
    for (unsigned i=2u ; i < rounds ; ++i) // use ++i instead of i++
    {
        x = -x; // some compilers optimize this better than x *= -1
        pi += (x / (2u * i - 1u)); // double / unsigned = double
    }
    
    pi *= 4;
    std::printf("%.16f\n", pi); // print 16 decimal digits of pi
}
