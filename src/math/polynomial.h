
#ifndef MATH_POLYNOMIAL_H
#define MATH_POLYNOMIAL_H

#include "types.h"
#include "math/constants.h"
#include "math/function.h"

/**
 * A polynomial in one variable.
 */
class Polynomial : public Function<double, double> {

    public:
	/// General constructor
	Polynomial(double* coefficients, uint num);
	
	/// Constructs a quartic polynomial
	Polynomial(double A, double B, double C, double D, double E);
	
	/// Constructs a cubic polynomial
	Polynomial(double A, double B, double C, double D);
	
	/// Constructs a quadratic polynomial
	Polynomial(double A, double B, double C);
	
	/// Constructs a linear polynomial
	Polynomial(double A, double B);

	/// Constructs a constant polynomial
	Polynomial(double A);

	/// Destructor
	virtual ~Polynomial();

	/// The highest power 
	uint order() const;

	/// Evaluate
	double eval(const double& x) const;

	/// Derivative
	Polynomial derivative() const;

	/// Comparator
	bool operator==(const Polynomial& p) const;

    private:
	void reduce();
	
	// Number of coefficients.
	uint num;
	// The coefficients. Eg. c[2]x^2 + c[1]x + c[0]
	double* coefficients;
};

#endif