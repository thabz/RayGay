#ifndef TESSALATION_H
#define TESSALATION_H

#include "mesh.h"

class Material;
class Vector;

/// An incremental tessalation of a Tetrahedron
class Tessalation : public Mesh {

public:
  /// Constructor
  Tessalation(const Vector center, const double radius, const uint32_t num,
              const Material *mat);

private:
  void split(const Vector &v1, const Vector &v2, const Vector &v3,
             uint32_t depth);

  uint32_t max_splits;
  double radius;
  Vector center;
};

#endif
