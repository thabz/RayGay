#ifndef MATH_FUNCTIONS_H
#define MATH_FUNCTIONS_H

/// A library of math functions
class Math {

    public:
	/// Calculate a binomial coefficient
	static unsigned long binomialCoefficient(long n, long k);
	
	/// Calculate a value of a Bernstein polynomial
	static double bernsteinPolynomial(unsigned int i, unsigned int n, double t);
};

#endif
