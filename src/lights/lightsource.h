
#ifndef LIGHTSOURCE_H
#define LIGHTSOURCE_H

#include "math/vector.h"
#include "rgb.h"
#include "lightinfo.h"

class RGB;
class Matrix;
class Intersection;
class Ray;
class Scene;
class SpaceSubdivider;

/// An interface lightsources must implement
class Lightsource {

    public:
	/// Shading info for this lightsource
	virtual Lightinfo getLightinfo(const Intersection& inter, const Vector& normal, const SpaceSubdivider& space) const = 0;
	/// Returns center of lightsource
	virtual const Vector& getPosition() const = 0;
	/// Apply a transformation
        virtual void transform(const Matrix& m) = 0;

};

#endif
