#include <fstream>
#include <iostream>

unsigned rounds;
double x = 1.0;
double pi = 1.0;

int main()
{
	std::ios::sync_with_stdio(false);   // cout speedhack
	
	std::ifstream infile("rounds.txt"); // open input file
	infile >> rounds; // read number from file
	infile.close(); // close input file
	
	rounds += 2u; // do this outside the loop
	
	for (unsigned i=2 ; i < rounds ; ++i) // use ++i instead of i++
	{
		x = -x; // some compilers optimize this better than x *= -1
		pi += (x / (2u * i - 1u)); // double / unsigned = double
	}
	
	pi *= 4;
	
	std::cout.precision(17); // set no. of decimal digits (upto 50)
	std::cout << pi << std::endl; // print pi to console
	return 0;
}
