#include "materials/plastic.h"

Plastic::Plastic(const RGB& color) : Material() {
    setDiffuseColor(color);
    setSpecularColor(RGB(1.0,1.0,1.0));
    setKs(0.8);
    setKd(0.0);
}

