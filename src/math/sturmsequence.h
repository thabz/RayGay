
#ifndef MATH_STURM_SEQUENCE_H
#define MATH_STURM_SEQUENCE_H

#include <vector>
#include "math/polynomial.h"

/**
 * A Sturm sequence.
 *
 * @see http://planetmath.org/encyclopedia/SturmSequence.html
 * @see http://mathworld.wolfram.com/SturmFunction.html
 */
class SturmSequence {

    public:
	/// Constructor
	SturmSequence(const Polynomial& polynomial);

	/// Calculate number of roots in the interval \f$ ]a,b[ \f$
	uint rootCount(double a, double b) const;

    private:
	/// Evaluate the sequence at a point
	void eval(double x, double* dest) const;

	/// Count number of sign changes at a point
	uint signChanges(double x) const;

	std::vector<Polynomial> f;
};

#endif

