
#ifndef MATERIALS_MARBLE_H
#define MATERIALS_MARBLE_H

#include "materials/material.h"

class Marble : public Material {
    public:
	Marble(const RGB& color1, const RGB& color2);
	RGB getDiffuseColor(const Intersection& i) const;

    private:
	RGB color1;
	RGB color2;
};

#endif

