
#ifndef POINTLIGHT_H 
#define POINTLIGHT_H

#include "lights/lightsource.h"

class RGB;
class Matrix;
class Intersection;
class Ray;

/// A point Lightsource
class Pointlight : public Lightsource {

    public:
	/// Constructor
        Pointlight(const Vector& pos);
	Lightinfo getLightinfo(const Intersection& inter, const Vector& normal, const SpaceSubdivider& space) const;
	const Vector& getPosition() const { return position; };

	void transform(const Matrix& m);
        RGB getDiffuseColor(const Vector& p);

    private:
	Vector position;
};

#endif
