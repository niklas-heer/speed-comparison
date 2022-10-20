#include <stdio.h>
#include <stdlib.h>

unsigned rounds;
double pi = 1.0;

int main()
{
    FILE *infile = fopen("rounds.txt", "r");   // open file
    if (infile == NULL) {
        perror("open file");
        return EXIT_FAILURE;
    }
    if (fscanf(infile, "%u", &rounds) != 1) {  // read from file
        perror("read file");
        return EXIT_FAILURE;
    }
    fclose(infile);                            // close file

    rounds += 2u; // do this outside the loop

    for (unsigned i=2u; i < rounds; ++i) // use ++i instead of i++
    {
        double x = -1.0 + 2.0 * (i & 0x1); // allows vectorization
        pi += (x / (2u * i - 1u)); // double / unsigned = double
    }

    pi *= 4;
    printf("%.16f\n", pi); // print 16 decimal digits of pi
    return EXIT_SUCCESS;
}
