#ifndef SPOTLIGHT_H
#define SPOTLIGHT_H

#include "lightsource.h"
#include "lightinfo.h"

class Matrix;

/// A directional Lightsource with a soft cutoff angle
class Spotlight : public Lightsource {

    public:
	/// Constructor
	Spotlight(const Vector& pos, const Vector& look_at, double angle, double cut_angle);
	Lightinfo getLightinfo(const Intersection& inter, const Vector& normal, SpaceSubdivider* space) const;
        void transform(const Matrix& m);

    private:
	Vector _dir;
	double _angle;
	double _cut_angle;
};

#endif
