
#include "materials/material.h"

/**
 * A checkerboard procedural texture.
 */
class Checker : public Material {

public:
  Checker(Material *mat1, Material *mat2, double size);
  RGB getDiffuseColor(const Intersection &i) const;

  // TODO: Override all methods

private:
  Material *materialAtPoint(const Vector &point) const;

  Material *mat1;
  Material *mat2;
  double size;
};
