
#ifndef MATH_MERSENNE_TWISTER_H
#define MATH_MERSENNE_TWISTER_H

#include "math/rng.h"

#define MT_LEN 624

/**
 * Mersenne Twister.
 *
 * Implemented and placed in the public domain by Michael Brundage.
 *
 * @see http://www.qbrundage.com/michaelb/pubs/essays/random_number_generation
 */
class MersenneTwister : public RNG {
public:
  MersenneTwister();

  /**
   * Reset the generator.
   */
  void reset();

  /**
   * Get next random number
   */
  unsigned long randomLong();

  virtual ~MersenneTwister(){};

private:
  int mt_index;
  unsigned long mt_buffer[MT_LEN];
};

#endif
