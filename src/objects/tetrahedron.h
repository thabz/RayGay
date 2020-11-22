#ifndef TETRAHEDRON_H
#define TETRAHEDRON_H

#include "mesh.h"

class Material;
class Vector;

/// A tetrahedron
/// TODO: Implement this simple object in Scheme instead
class Tetrahedron : public Mesh {

public:
  /// Constructor
  Tetrahedron(const Vector center, const double radius, const Material *mat);
};

#endif
