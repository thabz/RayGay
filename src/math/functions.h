#ifndef MATH_FUNCTIONS_H
#define MATH_FUNCTIONS_H

/// A library of math functions
class Math {

    public:
	/// Calculate a binomial coefficient
	static unsigned long binomialCoefficient(long n, long k);
	
	/// Calculate a value of a Bernstein polynomial
	static double bernsteinPolynomial(unsigned int i, unsigned int n, double t);
	/// Clamps a to the [0,1] interval.
	static double clamp(double a);
};

inline double Math::clamp(double a) {
    if (a > double(1)) { 
	return double(1);
    } else if (a < double(0)) {
	return double(0);
    } else {
	return a;
    }
}

#endif
