#ifndef TESSALATION_H
#define TESSALATION_H

#include "mesh.h"

class Material;
class Vector;

class Tessalation : public Mesh {

    public:
	Tessalation(const Vector center, const double radius, const unsigned int num, Material mat);

    private:
	void split(const Vector& v1, const Vector& v2, const Vector& v3, unsigned int depth);

	unsigned int max_splits;
	double radius;
	Vector center;
};

#endif
