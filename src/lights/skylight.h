#ifndef SKYLIGHT_H 
#define SKYLIGHT_H 

#include "lights/lightsource.h"
#include <vector>

class Object;

/// Hemispherical lightsource
class Skylight: public Lightsource {

    public:
	/// Constructor
	Skylight(double radius, int num);
	virtual ~Skylight() {};
	Lightinfo getLightinfo(const Intersection& inter, const Vector& normal, SpaceSubdivider* space) const;
        void transform(const Matrix& m) {};

    private:
	std::vector<Vector> positions;
	mutable std::vector<Object*> hints;
	double radius;
	int num;
};

#endif
