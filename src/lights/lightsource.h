
#ifndef LIGHTSOURCE_H
#define LIGHTSOURCE_H

#include "lightinfo.h"
#include "image/rgb.h"

class Matrix;
class Intersection;
class Ray;
class Scene;
class KdTree;
class Vector;
class Lightinfo;

/// An abstract class lightsources must extend 
class Lightsource {

    public:
	/// Constructor
	Lightsource(const Vector& center);
	
	/**
	 * Shading info for this lightsource.
	 *
	 * @param inter the intersection in question
	 * @param normal surface normal at the intersection
	 * @param space the space containing the objects of the scene
	 * @param info the structure to write the result into
	 * @param depth the number of times the ray has been reflected or refracted
	 */
	virtual void getLightinfo(const Intersection& inter, KdTree* space, Lightinfo* info, unsigned int depth) const = 0;
	
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

	/// Get the power of this light
	RGB getPower() const { return power; };
	/// Set the power of this light
	void setPower(const RGB& power) { this->power = power; };

    protected:
	/// Position of the lightsource
	Vector position;

    private:
	double fadeDistance;
	double fadePower;
	bool fadeEnabled;
	RGB power;
};

#endif
