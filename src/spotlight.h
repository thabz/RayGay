#ifndef SPOTLIGHT_H
#define SPOTLIGHT_H

#include "vector.h"
#include "lightsource.h"
#include "lightinfo.h"

class Matrix;

/// A directional Lightsource with a soft cutoff angle
class Spotlight : public Lightsource {

    public:
	/// Constructor
	Spotlight(const Vector& pos, const Vector& direction, double angle, double cut_angle);
	Lightinfo getLightinfo(const Intersection& inter, const Vector& normal, const Scene& scene) const;
	const Vector& getPosition() const { return _pos; };
        void transform(const Matrix& m);

    private:
	Vector _pos;
	Vector _dir;
	double _angle;
	double _cut_angle;
};

#endif
