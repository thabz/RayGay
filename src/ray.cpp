
#include "ray.h"

long Ray::seq = 0;

Ray::Ray() {
    id = ++seq;
    indice_of_refraction = 1.0;
    specularBounces = 0;
    diffuseBounces = 0;
}

Ray::Ray(const Vector& o, const Vector& d, const double indice) {
    origin = o;
    direction = d;
    indice_of_refraction = indice;
    inv_direction[0] = d[0] != 0.0 ? 1/d[0] : HUGE_DOUBLE;
    inv_direction[1] = d[1] != 0.0 ? 1/d[1] : HUGE_DOUBLE;
    inv_direction[2] = d[2] != 0.0 ? 1/d[2] : HUGE_DOUBLE;
    id = ++seq;
    specularBounces = 0;
    diffuseBounces = 0;
}

/// Says whether this ray has travelled a LS+ path
bool Ray::isCaustic() const {
    return diffuseBounces == 0 && specularBounces > 0;
}
