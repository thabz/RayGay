
#ifndef BRDF_H
#define BRDF_H

#include "math/vector.h"

/**
 * Abstract superclass of all BRDFs.
 *
 * Physically correct rendering systems describe reflection with
 * a <i>bidirectional reflectance distribution function</i>. A BRDF
 * is a function of a position on a surface and two directions, one
 * of incoming light and one of outgoing light.
 *
 *
 * The BRDF of an isotropic material is invariant under rotation
 * around the normal. All other materials are called anisotropic.
 *
 * @see http://www.fas.harvard.edu/~lib175/projects_fall_2000/vernal/ for
 * a examples of some of the bestknown BRDFs.
 */
class BRDF {

public:
  /// The bidirectional reflectance distribution function
  double f(const Vector &wi, const Vector &wo, const Vector &normal) = 0;

  /// The probability density function
  Vector pdf(const Vector &normal, const Vector &incoming) = 0;
};

#endif
