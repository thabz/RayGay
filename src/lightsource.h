
#ifndef LIGHTSOURCE_H
#define LIGHTSOURCE_H

#include "vector.h"
#include "rgb.h"
#include "lightinfo.h"

class RGB;
class Matrix;
class Intersection;
class Ray;
class Scene;

/// An interface lightsources must implement
class Lightsource {

    public:
	/// Shading info for this lightsource
	virtual Lightinfo getLightinfo(const Intersection& inter, const Vector& normal, const Scene& scene) const = 0;
	/// Returns center of lightsource
	virtual const Vector& getPosition() const = 0;
	/// Apply a transformation
        virtual void transform(const Matrix& m) = 0;

};

#endif
