#include "materials/plastic.h"

#include "math/vector2.h"
#include "intersection.h"
#include "objects/object.h"
#include "image/rgb.h"
#include "image/image.h"

Plastic::Plastic(const RGB& color) : Material() {
    setDiffuseColor(color);
    setSpecularColor(RGB(1.0,1.0,1.0));
    setKs(0.8);
    setKd(0.0);
}

