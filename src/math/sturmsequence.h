
#ifndef MATH_STURM_SEQUENCE_H
#define MATH_STURM_SEQUENCE_H

#include <vector>
#include "math/polynomial.h"

/**
 * A Sturm sequence.
 *
 * Jacques Charles Francois Sturm (1803-1855) found in 1829
 * a method for finding the number of real roots of a given
 * polynomial in a given interval.
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
	uint signChanges(double x) const;

	std::vector<Polynomial> f;
};

#endif

