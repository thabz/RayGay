

/**
 * Quasi-Monte-Carlo sequence
 */
class QMCSequence {

    public:
	virtual void reset() = 0;
	virtual double* getNext() = 0;
};
