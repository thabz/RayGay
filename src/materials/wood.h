
#ifndef MATERIALS_WOOD_H
#define MATERIALS_WOOD_H

#include "materials/material.h"

/**
 * A wood procedural texture.
 */
class Wood : public Material {
public:
  Wood(const RGB &color1, const RGB &color2);
  RGB getDiffuseColor(const Intersection &i) const;

private:
  RGB color1;
  RGB color2;
};

#endif
