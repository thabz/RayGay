
#ifndef MATERIALS_PLASTIC_H
#define MATERIALS_PLASTIC_H

#include "materials/material.h"

/**
 * A plastic material.
 */
class Plastic : public Material {

public:
  Plastic(const RGB &color);
};

#endif
