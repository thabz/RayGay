
#include "ray.h"

int64_t Ray::seq = 0;

Ray::Ray() {
    id = -1;
    indice_of_refraction = 1.0;
    specularBounces = 0;
    diffuseBounces = 0;
    t_scale = 1.0;
}

/// Says whether this ray has travelled a LS+ path
bool Ray::isCaustic() const {
    return diffuseBounces == 0 && specularBounces > 0;
}
