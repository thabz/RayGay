
#ifndef MATH_POLYNOMIAL_H
#define MATH_POLYNOMIAL_H

#include "types.h"
#include "math/constants.h"

/**
 * A polynomial in one variable.
 */
class Polynomial {

    public:
	Polynomial(double* coefficients, uint num);
	Polynomial(double A, double B, double C, double D);
	Polynomial(double A, double B, double C);
	Polynomial(double A, double B);
	Polynomial(double A);

	~Polynomial();

	/// The highest power 
	uint order() const;

	/// Evaluate
	double eval(double x) const;

	/// Derivative
	Polynomial derivative() const;

	/// Comparator
	bool operator==(const Polynomial& p) const;

    private:
	uint num;
	// Eg. c[2]x^2 + c[1]x + c[0]
	double* coefficients;
};

#endif
