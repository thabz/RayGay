
#include <iostream>
#include <cassert>
#include <cstdlib>
#include "math/polynomial.h"
#include "math/constants.h"

Polynomial::Polynomial() {
    num = 1;
    coefficients = coefficients_inline;
    coefficients[0] = 0.0;
}

Polynomial::Polynomial(const Polynomial& other) {
    init(other);
}

Polynomial::Polynomial(double* coefficients, uint32_t num) {
    assert(num >= 1);
    if (num < MAX_INLINE_COEFFS) {
	this->coefficients = coefficients_inline;
    } else {
	this->coefficients = new double[num];
    }
    for(uint32_t i = 0; i < num; i++) {
	this->coefficients[i] = coefficients[i];
    }
    this->num = num;
    reduce();
}

Polynomial::Polynomial(double A, double B, double C, double D, double E) {
    num = 5;
    coefficients = coefficients_inline;
    coefficients[0] = E;
    coefficients[1] = D;
    coefficients[2] = C;
    coefficients[3] = B;
    coefficients[4] = A;
    reduce();
}

Polynomial::Polynomial(double A, double B, double C, double D) {
    num = 4;
    coefficients = coefficients_inline;
    coefficients[0] = D;
    coefficients[1] = C;
    coefficients[2] = B;
    coefficients[3] = A;
    reduce();
}

Polynomial::Polynomial(double A, double B, double C) {
    num = 3;
    coefficients = coefficients_inline;
    coefficients[0] = C;
    coefficients[1] = B;
    coefficients[2] = A;
    reduce();
}

Polynomial::Polynomial(double A, double B) {
    num = 2;
    coefficients = coefficients_inline;
    coefficients[0] = B;
    coefficients[1] = A;
    reduce();
}

Polynomial::Polynomial(double A) {
    num = 1;
    coefficients = coefficients_inline;
    coefficients[0] = A;
}

Polynomial::~Polynomial() {
    if (coefficients != coefficients_inline && coefficients != NULL) {
    	delete [] coefficients;
	coefficients = NULL;
    }
}

uint32_t Polynomial::order() const {
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
    for(uint32_t i = 1; i < num; i++) {
	c[i-1] = coefficients[i] * i;
    }
    return Polynomial(c,num-1);
}

bool Polynomial::operator==(const Polynomial& p) const {
    if (p.num != num) return false;
    for(uint32_t i = 0; i < num; i++) {
	if (!IS_EQUAL(coefficients[i],p.coefficients[i]))
	    return false;
    }
    return true;
}

Polynomial Polynomial::operator+(const Polynomial& p) const {
    uint32_t new_num = MAX(p.num, num);
    double new_coefs[new_num];
    for(uint32_t i = 0; i < new_num; i++) {
	double q = 0.0;
	if (i < num) q += coefficients[i];
	if (i < p.num) q += p.coefficients[i];
	new_coefs[i] = q;
    }
    return Polynomial(new_coefs,new_num);
}

Polynomial Polynomial::operator-(const Polynomial& p) const {
    uint32_t new_num = MAX(p.num, num);
    double new_coefs[new_num];
    for(uint32_t i = 0; i < new_num; i++) {
	double q = 0.0;
	if (i < num) q += coefficients[i];
	if (i < p.num) q -= p.coefficients[i];
	new_coefs[i] = q;
    }
    return Polynomial(new_coefs,new_num);
}

Polynomial Polynomial::operator*(double c) const {
    double new_coefs[num];
    for(uint32_t i = 0; i < num; i++) {
	new_coefs[i] = coefficients[i] * c;
    }
    return Polynomial(new_coefs,num);
}

Polynomial Polynomial::operator/(double c) const {
    assert(IS_NZERO(c));
    double new_coefs[num];
    for(uint32_t i = 0; i < num; i++) {
	new_coefs[i] = coefficients[i] / c;
    }
    return Polynomial(new_coefs,num);
}

/**
 * This multiplies the polynomial by a power of x. 
 * It calculates \f$ g(x) = x^d f(x) \f$.
 *
 * @param d the \f$ d \f$ above.
 * @return the \f$ g(x) \f$ above.
 */
Polynomial Polynomial::timesX(uint32_t d) const {
    double new_coefficients[num + d];
    for(uint32_t i = 0; i < num+d; i++) {
	if (i < d) {
	    new_coefficients[i] = 0.0;
	} else {
	    new_coefficients[i] = coefficients[i-d]; 
	}
    }
    return Polynomial(new_coefficients, num + d);
}

double Polynomial::leadingCoefficient() const {
    return coefficients[num-1];
}

/**
 * Perform a polynomial long division. Finds the unique polynomials \f$ q(x) \f$ 
 * and \f$ r(x) \f$ such that
 *
 * \f[ \frac{f(x)}{d(x)} = q(x) + \frac{r(x)}{d(x)} \f]
 *
 * so that the order of \f$ r(x) \f$ is less than the order of \f$ d(x) \f$.
 *
 * The order of \f$ d(x) \f$ must be less than or equal to the order of 
 * \f$ f(x) \f$ and \f$ d(x) \ne 0 \f$.
 * 
 * @param divisor the \f$ d(x) \f$ above.
 * @param remainder \f$ r(x) \f$ above is written here.
 * @return the quotient \f$ q(x) \f$  above.
 *
 * @see http://www.sosmath.com/algebra/factor/fac01/fac01.html
 */
Polynomial Polynomial::division(const Polynomial& divisor, Polynomial& remainder) const {
    assert(!(divisor.order() == 0 && IS_ZERO(divisor.coefficients[0])));

    if (divisor.order() == 0) {
	remainder = Polynomial(0);
	return *this / divisor.coefficient(0);
    }

    double quotient_coeffs[num];
    for(uint32_t i = 0; i < num; i++) quotient_coeffs[i] = 0.0;

    double div_lead_q = divisor.leadingCoefficient();
    uint32_t div_lead_d = divisor.order();
    remainder = *this;
    Polynomial remainder_2;

    do {
	uint32_t new_d = remainder.order() - div_lead_d;
	double new_q = remainder.leadingCoefficient() / div_lead_q;
	quotient_coeffs[new_d] = new_q;
	Polynomial remainder_2 = divisor.timesX(new_d) * new_q;
	remainder = remainder - remainder_2;
    } while (remainder.order() >= div_lead_d);

    remainder.reduce();
    return Polynomial(quotient_coeffs,num);
}


/**
 * Decrease order when leading coefficients are zero.
 */
void Polynomial::reduce() {
    while (num > 1 && IS_ZERO(coefficients[num-1])) {
	num--;
    }
}

Polynomial Polynomial::operator*(const Polynomial& other) const {
    double result_coeffs[num+other.num];

    for(uint32_t i = 0; i < num+other.num; i++) {
	result_coeffs[i] = 0.0;
    }

    for(uint32_t i = 0; i < num; i++) {
	for(uint32_t j = 0; j < other.num; j++) {
	    result_coeffs[i+j] += other.coefficients[j] * coefficients[i];
	}
    }

    return Polynomial(result_coeffs, num + other.num);
}

Polynomial& Polynomial::operator=(const Polynomial& other) {
    if (this != &other) {
	init(other);
    }
    return *this;
}

// This is used by both copyconstructor and assignment operator
void Polynomial::init(const Polynomial& other) {
    num = other.num;
    if (num < MAX_INLINE_COEFFS) {
	this->coefficients = coefficients_inline;
    } else {
	// Potential leak of existing coefficients here
	this->coefficients = new double[num];
    }
    for(uint32_t i = 0; i < num; i++) {
	coefficients[i] = other.coefficients[i];
    }
    reduce();
}

std::ostream & operator<<(std::ostream &os, const Polynomial &p) {
    os << "(";
    for(uint32_t i = 0; i <= p.order(); i++) {
	os << p.coefficient(p.order() - i);
	if (i != p.order())
	    os << ",";
    }
    os << ")";
    return os;
}

