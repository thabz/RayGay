#ifndef MATH_FUNCTIONS_H
#define MATH_FUNCTIONS_H

#include "types.h"
#include <vector>

class Vector;
class QMCSequence;
class Vector2;

/// A library of math functions
class Math {

    public:
	/// Calculate a binomial coefficient
	static long binomialCoefficient(long n, long k);
	
	/// Calculate a value of a Bernstein polynomial
	static double bernsteinPolynomial(uint32_t i, uint32_t n, double t);
	/// Clamps a to the [0,1] interval.
	static double clamp(double a);

	/// Solves quartic equation
	static int solveQuartic(double A, double B, double C, double D, double* roots);
	
	/// Solves quartic equation
	static int solveQuarticSingle(double A, double B, double C, double D, double cut, double* root);

	/// Solves quartic equation (invalid)
	static int solveQuartic_Schaum(double A, double B, double C, double D, double* roots);
	
	/// Solves cubic equation
	static int solveCubic(double A, double B, double C, double* roots);
	
	/// Solves cubic equation and return smallest root
	static double solveCubicSingle(double A, double B, double C, double* root);

	/// Solves quadratic equation
	static int solveQuadratic(double A, double B, double C, double* roots);

	static Vector perturbVector(const Vector& axis, const double angle);
	static Vector perturbVector(const Vector& axis, const double angle, QMCSequence* qmc_sequence);

	static Vector2 shirleyDisc(double seedx, double seedy);
	
        static std::vector<Vector> toUnitSphere(const std::vector<Vector2>& points);
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
