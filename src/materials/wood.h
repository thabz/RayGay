
#ifndef MATERIALS_WOOD_H
#define MATERIALS_WOOD_H

#include "materials/material.h"

class Wood : public Material {
    public:
	Wood(const RGB& color1, const RGB& color2);
	RGB getDiffuseColor(const Intersection& i) const;
}

#endif
