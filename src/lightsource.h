
#ifndef LIGHTSOURCE_H
#define LIGHTSOURCE_H

#include "vector.h"
#include "rgb.h"

class RGB;
class Matrix;
class Intersection;
class Ray;

/// A point lightsource
class Lightsource {

    public:
        Lightsource(Vector pos);
        virtual ~Lightsource();
	virtual void transform(const Matrix& m);
        virtual Intersection intersect(const Ray& ray);
        virtual RGB getDiffuseColor(const Vector& p);
	virtual Vector getPosition() { return position; };

    private:
	Vector position;
};

#endif
