#include <iostream>
#include <string>
#include <fstream>
#include <streambuf>
#include <limits>

int main()
{
    // Needed to automatically use the max. precision
    typedef std::numeric_limits< double > dbl;

    // Read rounds from file
    std::ifstream t("rounds.txt");
    std::string str((std::istreambuf_iterator<char>(t)),
                    std::istreambuf_iterator<char>());

    int rounds = std::stoi(str);  // convert to int

    double x = 1.0;
    double pi = 1.0;

    for (int i = 2; i < rounds + 2; i++) {
        x *= -1;
        pi += (x / (2.0 * i - 1.0));
    }

    pi *= 4;

    std::cout.precision(dbl::max_digits10);  // see: https://stackoverflow.com/a/554134
    std::cout << pi << std::endl;

    return 0;
}
