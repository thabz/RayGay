
#ifndef MATH_HALTON_H
#define MATH_HALTON_H

#include "math/qmcsequence.h"
#include <pthread.h>

/**
 * The Halton sequence.
 *
 * @see http://www.math.iastate.edu/reu/2001/voronoi/halton_sequence.html
 */
class Halton : public QMCSequence {

public:
  Halton(int base, int dim);
  virtual ~Halton();
  void reset(double seed = 0);
  double *getNext();

private:
  int nextPrime(int p) const;

  double *invBase;
  double *values;
  int dim;
  mutable pthread_mutex_t mutex;
};

#endif
