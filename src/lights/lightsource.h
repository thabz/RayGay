
#ifndef LIGHTSOURCE_H
#define LIGHTSOURCE_H

#include "lightinfo.h"

class RGB;
class Matrix;
class Intersection;
class Ray;
class Scene;
class SpaceSubdivider;
class Vector;
class RGB;
class Lightinfo;

/// An abstract class lightsources must extend 
class Lightsource {

    public:
	/// Constructor
	Lightsource(const Vector& center);
	
	/// Shading info for this lightsource
	virtual Lightinfo getLightinfo(const Intersection& inter, const Vector& normal, SpaceSubdivider* space) const = 0;
	
	/// Apply a transformation
        virtual void transform(const Matrix& m);

	/// Set attenuation parameters
	void setAttenuation(double fadeDistance, double fadePower);

	/// Get attenuation factor at a point in space
	double getAttenuation(const Vector& point) const;

	/// Returns center of lightsource
	const Vector getPosition() const { return position; };

	/// Shoot a photon ray from this lightsource
	virtual Ray getRandomPhotonRay() const;

    protected:
	/// Position of the lightsource
	Vector position;

    private:
	double fadeDistance;
	double fadePower;
	bool fadeEnabled;
};

#endif
