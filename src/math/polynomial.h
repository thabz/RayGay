
#ifndef MATH_POLYNOMIAL_H
#define MATH_POLYNOMIAL_H

#include <iosfwd>
#include "types.h"
#include "math/constants.h"
#include "math/function.h"

#define MAX_INLINE_COEFFS 5

/**
 * A polynomial in one variable.
 *
 * TODO: Optimer for polynomials med num under f.eks. 6 ved at
 * have
 *
 * union {
 *     double local_coefficients[6],
 *     double* coefficients
 * }
 *
 * og ved at kun at tilgå disse via getCoefficient(uint i)
 * som udfra num afgør om hvordan coefficienten skal findes.
 */
class Polynomial : public Function<double, double> {
    friend std::ostream& operator<< (std::ostream& os, const Polynomial& x);

    public:
	/// Default constructor
	Polynomial();

	/// Copy constructor
	Polynomial(const Polynomial& other);

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

	/// The leading coefficient
	double leadingCoefficient() const;

	/// Evaluate
	double eval(const double& x) const;

	/// Derivative
	Polynomial derivative() const;

	/// Multiply by a power of x
	Polynomial timesX(uint degree) const;

	/// Comparator
	bool operator==(const Polynomial& p) const;

	Polynomial operator+(const Polynomial& p) const;
	Polynomial operator-(const Polynomial& p) const;
	Polynomial operator*(const Polynomial& p) const;
	Polynomial& operator=(const Polynomial& p);
	Polynomial operator*(double c) const;
	Polynomial operator/(double c) const;

	/// Polynomial long division
	Polynomial division(const Polynomial& divisor, Polynomial& remainder) const;
	double coefficient(uint i) const { return coefficients[i]; };

    private:
	void init(const Polynomial& other);
	void reduce();
	
	// Number of coefficients.
	uint num;

	// The coefficients. Eg. c[2]x^2 + c[1]x + c[0]
	double* coefficients;
	double coefficients_inline[MAX_INLINE_COEFFS];
};

#endif
