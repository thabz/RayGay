#ifndef TESSALATION_H
#define TESSALATION_H

#include "mesh.h"

class Material;
class Vector;

/// An incremental tessalation of a Tetrahedron
class Tessalation : public Mesh {

    public:
	/// Constructor
	Tessalation(const Vector center, const double radius, const uint num, const Material* mat);

    private:
	void split(const Vector& v1, const Vector& v2, const Vector& v3, uint depth);

	uint max_splits;
	double radius;
	Vector center;
};

#endif
