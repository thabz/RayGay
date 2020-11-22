
#ifndef MATH_POISSON_DISC
#define MATH_POISSON_DISC

#include "math/interval.h"
#include "math/vector2.h"
#include <vector>

class PoissonDiscDistribution {
public:
  PoissonDiscDistribution(double w, double h);

  /**
   * Creates a set of random points within [0,w] &cross; [0,h] that
   * all are at least a distance of 2r from each other.
   *
   * @param result must have room for num Vector2's
   * @return the number of points found which is &leq; num
   */
  int createSet(double r, uint32_t num, Vector2 *result);

private:
  void dartThrowing(double r, uint32_t num);
  void boundarySampling(double r, uint32_t num);

  std::vector<Vector2> &getNeighbours(const Vector2 &p);
  void add(const Vector2 &p, double r);

  double w, h;
  std::vector<Vector2> all;
};

#endif
