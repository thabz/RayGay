#ifndef SKYLIGHT_H 
#define SKYLIGHT_H 

#include "lights/lightsource.h"
#include "lights/shadowcache.h"
#include <vector>

class Object;

/// Hemispherical lightsource
class Skylight: public Lightsource {

    public:
	/// Constructor
	Skylight(double radius, int num);
	virtual ~Skylight() {};
	void getLightinfo(const Intersection& inter, const Vector& normal, SpaceSubdivider* space, Lightinfo* info, unsigned int depth) const;
        void transform(const Matrix& m) {};

    private:
	std::vector<Vector> positions;
	mutable std::vector<ShadowCache> shadowcaches;
	double radius;
	int num;
};

#endif
