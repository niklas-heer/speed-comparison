#import <stdio.h>
#import <stdlib.h>

int main(int argc, const char * argv[]) {
    FILE *file = fopen("rounds.txt", "r");
    if (!file) {
        fprintf(stderr, "Error: Cannot open rounds.txt\n");
        return 1;
    }

    long rounds;
    fscanf(file, "%ld", &rounds);
    fclose(file);

    double x = 1.0;
    double pi = 1.0;

    for (long i = 2; i <= rounds + 2; i++) {
        x *= -1.0;
        pi += x / (2.0 * i - 1.0);
    }

    pi *= 4.0;

    printf("%.17g", pi);
    return 0;
}
