
#include "math/qmcsequence.h"

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
