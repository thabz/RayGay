
#ifndef MATH_HALTON_H
#define MATH_HALTON_H

#include "math/qmcsequence.h"

/**
 * The Halton sequence.
 * 
 * @see http://www.math.iastate.edu/reu/2001/voronoi/halton_sequence.html
 */
class Halton : public QMCSequence {

    public:
	Halton(int base, int dim);
	void reset();
	double* getNext();

    private:
	int nextPrime(int p) const;

	double* invBase;
	double* values;
	int dim;
};

#endif
