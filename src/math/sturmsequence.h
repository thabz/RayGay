
#ifndef MATH_STURM_SEQUENCE_H
#define MATH_STURM_SEQUENCE_H

#include <vector>
#include "math/polynomial.h"

/**
 * A Sturm sequence.
 *
 * In 1829 Jacques Charles Francois Sturm (1803-1855) found 
 * a method for finding the number of real roots of a given
 * polynomial in a given interval.
 *
 * Let \f$ P(x) \f$ be a polynomial and let \f$ P'(x) \f$ be the derivative
 * of \f$ P \f$ with respect to \f$ x \f$. Then we define a sequence of
 * functions \f$ P_i \f$ as follows:
 *
 * \f[ P_0 = P \f]
 * \f[ P_1 = P' \f]
 * \f[ P_i = - ( P_{i-2} mod  P_{i-1}  ) \f]
 * \f[ P_n = 0 \f]
 *
 * The last part is simply the remainder of the polynomial \f$ P_{i-2} \f$
 * upon division by the polynomial \f$ P_{i-1} \f$. Also note that
 * \f$ \deg P_i < \deg P_{i-1} \f$ so the sequence does end.
 *
 * For any real value \f$ u \f$, consider the sequence of values 
 * \f$ P_i(u) \f$ for all \f$ i \f$. Let \f$ F(u) \f$ be the number 
 * of times this sequence changes sign, for a particular value of 
 * \f$ u \f$. Now, Sturm's theorem says that the number of distinct 
 * roots of \f$ P \f$ in the interval \f$ ]a,b[ \f$ is the absolute value of 
 * \f$ F(b) - F(a) \f$.
 *
 * This is an implementation of that method.
 *
 * @see http://planetmath.org/encyclopedia/SturmSequence.html
 * @see http://mathworld.wolfram.com/SturmFunction.html
 * @see http://aida.homelinux.net/wordpress/wp-content/522_ma/ma522_01.html
 */
class SturmSequence {

    public:
	/// Constructor
	SturmSequence(const Polynomial& polynomial);

	/// Calculate number of distinct real roots in the interval \f$ ]a,b[ \f$
	uint rootCount(double a, double b) const;

    private:
	/// Evaluate the sequence at a point
	void eval(double x, double* dest) const;

	/// Count number of sign changes at a point
	int signChanges(double x) const;

	std::vector<Polynomial> f;
};

#endif

