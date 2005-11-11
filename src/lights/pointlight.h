
#ifndef POINTLIGHT_H 
#define POINTLIGHT_H

#include "lights/lightsource.h"
#include "lights/shadowcache.h"

class RGB;
class Matrix;
class Intersection;
class Object;

/// A point Lightsource
class Pointlight : public Lightsource {

    public:
	/// Constructor
        Pointlight(const Vector& pos);
	void getLightinfo(const Intersection& inter, KdTree* space, Lightinfo* info, uint32_t depth) const;

    private:
	mutable ShadowCache shadowcache;
};

#endif
