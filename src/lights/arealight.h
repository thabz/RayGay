#ifndef AREALIGHT_H
#define AREALIGHT_H

#include "lights/lightsource.h"
#include "lights/shadowcache.h"
#include <vector>

class Circle;
class Object;

/// Lightsource that produces soft shadows
class Arealight : public Lightsource {

    public:
	/// Constructor
	Arealight(const Vector& pos, const Vector& dir, double radius, int num, double jitter);
	virtual ~Arealight();
	void getLightinfo(const Intersection& inter, SpaceSubdivider* space, Lightinfo* info, unsigned int depth) const;
        void transform(const Matrix& m);

    private:
	std::vector<Circle*> circles;
	mutable std::vector<ShadowCache> shadowcaches;
	std::vector<double> ts;
	double jitter;
	int num;
	Vector getPosition(int i) const;
};

#endif
