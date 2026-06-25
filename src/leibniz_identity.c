#include <stdio.h>
#include <stdlib.h>

unsigned rounds;

int main(void)
{
    FILE *infile = fopen("rounds.txt", "r");
    if (infile == NULL)
    {
        perror("open file");
        return EXIT_FAILURE;
    }

    if (fscanf(infile, "%u", &rounds) != 1)
    {
        perror("read file");
        fclose(infile);
        return EXIT_FAILURE;
    }

    fclose(infile);

    double sum1 = 0.0;
    double sum2 = 0.0;
    double sum3 = 0.0;
    double sum4 = 0.0;

    unsigned remaining = rounds + 1u;
    double d = 3.0;

    while (remaining >= 16u)
    {
        sum1 += -2.0 / (d * (d + 2.0)) + -2.0 / ((d + 4.0) * (d + 6.0));

        sum2 += -2.0 / ((d + 8.0) * (d + 10.0)) + -2.0 / ((d + 12.0) * (d + 14.0));

        sum3 += -2.0 / ((d + 16.0) * (d + 18.0)) + -2.0 / ((d + 20.0) * (d + 22.0));

        sum4 += -2.0 / ((d + 24.0) * (d + 26.0)) + -2.0 / ((d + 28.0) * (d + 30.0));

        d += 32.0;
        remaining -= 16u;
    }

    double pi = 1.0 + sum1 + sum2 + sum3 + sum4;

    while (remaining >= 2u)
    {
        pi -= 2.0 / (d * (d + 2.0));
        d += 4.0;
        remaining -= 2u;
    }

    if (remaining > 0u)
    {
        pi -= 1.0 / d;
    }

    pi *= 4.0;

    printf("%.16f\n", pi);

    return EXIT_SUCCESS;
}