
#include "math/vector.h"

/**
 * The BRDF of an isotropic material is invariant under rotation
 * around the normal. All other materials are called anisotropic.
 */
class BRDF {

    double brdf(const Vector& wi, const Vector& wo, const Vector& normal) = 0;

    /* Probability density function */
    Vector pdf(const Vector& normal, const Vector& incoming) = 0;
}
