#ifndef TETRAHEDRON_H
#define TETRAHEDRON_H

#include "mesh.h"

class Material;
class Vector;

/// A tetrahedron
class Tetrahedron : public Mesh {

    public:
	/// Constructor
	Tetrahedron(const Vector center, const double radius, Material mat);
};

#endif



