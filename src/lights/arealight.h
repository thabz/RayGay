#ifndef AREALIGHT_H
#define AREALIGHT_H

#include "lights/lightsource.h"
#include <vector>

class Circle;
class Object;

/// Lightsource that produces soft shadows
class Arealight : public Lightsource {

    public:
	/// Constructor
	Arealight(const Vector& pos, const Vector& dir, double radius, int num, double jitter);
	virtual ~Arealight();
	Lightinfo getLightinfo(const Intersection& inter, const Vector& normal, SpaceSubdivider* space) const;
        void transform(const Matrix& m);

    private:
	std::vector<Circle*> circles;
	mutable std::vector<Object*> hints;
	std::vector<double> ts;
	double jitter;
	int num;
	Vector getPosition(int i) const;
};

#endif
