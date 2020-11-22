
#ifndef OBJECTS_WIREFRAME_H
#define OBJECTS_WIREFRAME_H

#include "objectgroup.h"

class Mesh;
class Material;

/// A solid wireframe representation of a Mesh.
// The vertices are represented as cylinders and the vertices as spheres.
class Wireframe : public ObjectGroup {

public:
  /// Constructor
  Wireframe(Mesh *mesh, double radius, const Material *material);
};

#endif
