#ifndef SPOTLIGHT_H
#define SPOTLIGHT_H

#include "lightsource.h"

class Matrix;
class QMCSequence;

/// A directional Lightsource with a soft cutoff angle
class Spotlight : public Lightsource {

    public:
	/// Constructor
	Spotlight(const Vector& pos, const Vector& look_at, double angle, double cut_angle);
	virtual ~Spotlight();
	void getLightinfo(const Intersection& inter, const Vector& normal, SpaceSubdivider* space, Lightinfo* info, unsigned int depth) const;
	void transform(const Matrix& m);
	Ray getRandomPhotonRay() const;

    private:
	Vector _dir;
	double _angle;
	double _cut_angle;
	QMCSequence* _qmc;
};

#endif
