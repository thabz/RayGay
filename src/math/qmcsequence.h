
#ifndef MATH_QMC_SEQUENCE
#define MATH_QMC_SEQUENCE

/**
 * Quasi-Monte-Carlo sequence
 */
class QMCSequence {

    public:
	/**
	 * Reset the sequence.
	 *
	 * There is no guarantee that the sequence will be
	 * repeated after a reset.
	 */
	virtual void reset(double seed = 0) = 0;

	/**
	 * Get next random numbers
	 *
	 * @return a pointer to dim doubles in [0,1]
	 */
	virtual double* getNext() = 0;

	virtual ~QMCSequence() {};
};

#endif
