
#include "wood.h"
#include "perlin.h"
#include "intersection.h"

Wood::Wood(const RGB& color1, const RGB& color2) : Material() {
    this->color1 = color1;
    this->color2 = color2;
}

RGB Wood::getDiffuseColor(const Intersection& i) const {
    Vector p = i.getPoint() / 250;
    p[1] = p[1] / 10.0;
    float vec[3];
    vec[0] = p[0]; vec[1] = p[1]; vec[2] = p[2];
    float g = noise3(vec) * 20;
    g = g - floor(g);
 //   g = (1.0 + g) / 2.0;
    return g*color1 + (1.0-g)*color2;
    return RGB(g,g,g);
}

