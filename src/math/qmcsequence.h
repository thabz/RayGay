

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
	virtual void reset() = 0;

	/**
	 * Get next random numbers
	 *
	 * @return a pointer to dim doubles in [0,1]
	 */
	virtual double* getNext() = 0;
};
