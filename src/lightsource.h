
#ifndef LIGHTSOURCE_H
#define LIGHTSOURCE_H

#include "vector.h"
#include "rgb.h"

class RGB;
class Matrix;
class Intersection;
class Ray;

/**
 * An interface lightsources must implement
 */
class Lightsource {

    public:
	virtual double getIntensity(const Vector& direction_to_light, double cos) const = 0;
	virtual const Vector& getPosition() const = 0;
        virtual void transform(const Matrix& m) = 0;

};

#endif
