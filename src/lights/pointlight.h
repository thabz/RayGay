
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
	Lightinfo getLightinfo(const Intersection& inter, const Vector& normal, SpaceSubdivider* space, unsigned int depth) const;

    private:
	mutable ShadowCache shadowcache;
};

#endif
