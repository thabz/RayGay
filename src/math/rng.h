
#ifndef MATH_RNG_H
#define MATH_RNG_H

#include <climits>

/**
 * Random Number Generator
 * TODO: Mersenne Twister et. al. are not threadsafe and some uses big
 * statetables that thrashes the CPU cache.
 * http://www.acm.org/tog/resources/RTNews/html/rtnv20n1.html#art10
 */
class RNG {
public:
  /**
   * Reset the generator.
   */
  virtual void reset() = 0;

  /**
   * Get next random number as a unsigned long.
   */
  virtual unsigned long randomLong() = 0;

  /**
   * Get next random number as a double in [0;1]
   */
  double randomDouble();

  virtual ~RNG(){};
};

double RNG::randomDouble() { return double(randomLong()) / double(ULONG_MAX); }

#endif
