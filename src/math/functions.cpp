
#include "math/functions.h"
#include "math/vector.h"

/**
 * The binomial coefficient is defined as 
 * \f$ {n \choose k} = \frac{n!}{k!(n-k)!} \quad \mbox{if } n\geq k\geq 0 \qquad \mbox{(1)} \f$
 *  \f$ {n \choose k} = 0 \quad \mbox{if } k<0 \mbox{ or } k>n. \f$
 * 
 * Implemented as a slow recursive function.
 *
 * See http://www.brpreiss.com/books/opus4/html/page467.html for a O(n^2) version.
 */
unsigned long Math::binomialCoefficient(long n, long k) {
    if (k == 0 || k == n) {
	return 1;
    } else if (k < 0 || k > n) {
	return 0;
    } else {
        return binomialCoefficient(n-1,k-1) + binomialCoefficient(n-1,k);
    }
}

double Math::bernsteinPolynomial(unsigned int i, unsigned int n, double t) {
    return binomialCoefficient(n,i) * pow(t,i) * pow((1-t),n-i);
}
