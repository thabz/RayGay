
#ifndef MATERIALS_PLASTIC_H
#define MATERIALS_PLASTIC_H

#include "materials/material.h"

class Plastic : public Material {

    public:
	Plastic(const RGB& color);
}

Plastic::Plastic(const RGB& color) : Material() {
    setDiffuseColor(color);
    setSpecularColor(RGB(1.0,1.0,1.0));
    setKs(0.8);
    setKd(0.0);
}

#endif
