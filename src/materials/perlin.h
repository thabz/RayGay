#ifndef PERLIN_H
#define PERLIN_H

#include "math/vector.h"

class Perlin {

    public:
	static double noise(const Vector& pos);
	static Vector noise3d(const Vector& pos, double offset);
};

#endif
