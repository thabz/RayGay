
#ifndef POINTLIGHT_H 
#define POINTLIGHT_H

#include "vector.h"
#include "rgb.h"
#include "lightsource.h"

class RGB;
class Matrix;
class Intersection;
class Ray;

/// A point Lightsource
class Pointlight : public Lightsource {

    public:
        Pointlight(const Vector& pos);
	double getIntensity(const Vector& direction_to_light, double cos) const;
	const Vector& getPosition() const { return position; };

	void transform(const Matrix& m);
        RGB getDiffuseColor(const Vector& p);

    private:
	Vector position;
};

#endif
