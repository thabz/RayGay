
#include "irradiancecache.h"

/**
 * The weight function.
 *
 * Using Henrik Wann Jensens interpretation.
 */
double IrradianceCache::CacheNode::getWeight(const Vector& x, const Vector& n) const {
    double d1 = (x - point).length() / hmd;
    double d2 = sqrt(1.0 - n*normal);
    return 1.0 / (d1 + d2);
}
