
#include "wood.h"
#include "perlin.h"
#include "intersection.h"

Wood::Wood(const RGB& color1, const RGB& color2) : Material() {
    this->color1 = color1;
    this->color2 = color2;
}

RGB Wood::getDiffuseColor(const Intersection& i) const {
    Vector p = i.getPoint();
    float vec[3];
    vec[0] = p[0];vec[1] = p[1];vec[1] = p[1];
    float g = noise3(vec) * 20;
    g = g - int(g);

    return g*color1 + (1-g)*color2;
}

