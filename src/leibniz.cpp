#include <iostream>
#include <fstream>

int main()
{
    long rounds{0};

    std::ifstream infile("rounds.txt");   // open file
    if (!(infile >> rounds))
    {
        perror("read file");
        return EXIT_FAILURE;
    }
    infile.close();                            // close file

    double x {1.0};
    double pi {1.0};
    
    rounds += 2; // do this outside the loop
    
    for (long i {2} ; i < rounds ; i++) // use ++i instead of i++
    {
        x = -1.0 + 2.0 * (i & 0x1); // allows vectorization
        pi += (x / (2 * i - 1)); // double / unsigned = double
    }
    
    pi *= 4;
    std::cout.precision(16);
    std::cout << pi; // print 16 decimal digits of pi
    return EXIT_SUCCESS;
}
