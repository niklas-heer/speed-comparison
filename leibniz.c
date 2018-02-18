#include <stdio.h>
#include <stdlib.h>
#include <string.h>


int main()
{
    // TODO: make the precision better.

    // Read the file into a string
    char *file_contents;
    long input_file_size;
    FILE *input_file = fopen("rounds.txt", "rb");
    fseek(input_file, 0, SEEK_END);
    input_file_size = ftell(input_file);
    rewind(input_file);
    file_contents = malloc(input_file_size * (sizeof(char)));
    fread(file_contents, sizeof(char), input_file_size, input_file);
    fclose(input_file);

    int rounds = atoi(file_contents);    // convert into an int

    double x = 1.0;
    double pi = 1.0;

    for (int i = 2; i < rounds + 2; i++) {
        x *= -1;
        pi += (x / (2.0 * i - 1.0));
    }

    pi *= 4;

    printf("π = %f\n", pi);
}


