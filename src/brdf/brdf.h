
#include "math/vector.h"

/**
 * The BRDF of an isotropic material is invariant under rotation
 * around the normal. All other materials are called anisotropic.
 *
 * @see http://www.fas.harvard.edu/~lib175/projects_fall_2000/vernal/
 */
class BRDF {

    double brdf(const Vector& wi, const Vector& wo, const Vector& normal) = 0;

    /* Probability density function */
    Vector pdf(const Vector& normal, const Vector& incoming) = 0;
}
