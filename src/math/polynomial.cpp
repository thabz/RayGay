
#include <cassert>
#include <cstdlib>
#include "math/polynomial.h"
#include "math/constants.h"

Polynomial::Polynomial(double* coefficients, uint num) {
    assert(num >= 1);
    this->coefficients = new double[num];
    for(uint i = 0; i < num; i++) {
	this->coefficients[i] = coefficients[i];
    }
    this->num = num;
    reduce();
}

Polynomial::Polynomial(double A, double B, double C, double D, double E) {
    num = 5;
    coefficients = new double[num];
    coefficients[0] = E;
    coefficients[1] = D;
    coefficients[2] = C;
    coefficients[3] = B;
    coefficients[4] = A;
    reduce();
}

Polynomial::Polynomial(double A, double B, double C, double D) {
    num = 4;
    coefficients = new double[num];
    coefficients[0] = D;
    coefficients[1] = C;
    coefficients[2] = B;
    coefficients[3] = A;
    reduce();
}

Polynomial::Polynomial(double A, double B, double C) {
    num = 3;
    coefficients = new double[num];
    coefficients[0] = C;
    coefficients[1] = B;
    coefficients[2] = A;
    reduce();
}

Polynomial::Polynomial(double A, double B) {
    num = 2;
    coefficients = new double[num];
    coefficients[0] = B;
    coefficients[1] = A;
    reduce();
}

Polynomial::Polynomial(double A) {
    num = 1;
    coefficients = new double[num];
    coefficients[0] = A;
}

Polynomial::~Polynomial() {
    delete [] coefficients;
}

uint Polynomial::order() const {
    return num - 1;
}

/**
 * Evaluate the polynomial at a point.
 *
 * Limts number of multiplications by utilizing Horner's rule:
 *
 * \f[ a_nx^n + a_{n-1}x^{n-1} + \cdots + a_0 = (( a_nx + a_{n-1})x + \cdots) x + a_0  \f]
 *
 * @see http://mathworld.wolfram.com/HornersRule.html
 */
double Polynomial::eval(const double& x) const {
    double result = coefficients[num-1];
    for(int i = num - 2; i >= 0; i--) {
	result = result * x + coefficients[i];
    }
    return result;
}

Polynomial Polynomial::derivative() const {
    if (num == 1) 
	return Polynomial(0.0);

    double c[num-1];
    for(uint i = 1; i < num; i++) {
	c[i-1] = coefficients[i] * i;
    }
    return Polynomial(c,num-1);
}

bool Polynomial::operator==(const Polynomial& p) const {
    if (p.num != num) return false;
    for(uint i = 0; i < num; i++) {
	if (!IS_EQUAL(coefficients[i],p.coefficients[i]))
	    return false;
    }
    return true;
}

Polynomial Polynomial::operator+(const Polynomial& p) const {
    uint new_num = MAX(p.num, num);
    double* new_coefs = (double*)alloca(new_num * sizeof(double));
    for(uint i = 0; i < new_num; i++) {
	double q = 0.0;
	if (i < num) q += coefficients[i];
	if (i < p.num) q += p.coefficients[i];
	new_coefs[i] = q;
    }
    return Polynomial(new_coefs,new_num);
}

Polynomial Polynomial::operator-(const Polynomial& p) const {
    uint new_num = MAX(p.num, num);
    double* new_coefs = (double*)alloca(new_num * sizeof(double));
    for(uint i = 0; i < new_num; i++) {
	double q = 0.0;
	if (i < num) q += coefficients[i];
	if (i < p.num) q -= p.coefficients[i];
	new_coefs[i] = q;
    }
    return Polynomial(new_coefs,new_num);
}

Polynomial Polynomial::operator*(double c) const {
    double* new_coefs = (double*)alloca(num * sizeof(double));
    for(uint i = 0; i < num; i++) {
	new_coefs[i] = coefficients[i] * c;
    }
    return Polynomial(new_coefs,num);
}

Polynomial Polynomial::operator/(double c) const {
    assert(!IS_ZERO(c));
    double* new_coefs = (double*)alloca(num * sizeof(double));
    for(uint i = 0; i < num; i++) {
	new_coefs[i] = coefficients[i] / c;
    }
    return Polynomial(new_coefs,num);
}

/**
 * Decrease order when leading coefficients are zero.
 */
void Polynomial::reduce() {
    while (num > 1 && IS_ZERO(coefficients[num-1])) {
	num--;
    }
}

