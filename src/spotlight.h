#ifndef SPOTLIGHT_H
#define SPOTLIGHT_H

#include "vector.h"
#include "lightsource.h"

class Matrix;

/// A directional Lightsource with a soft cutoff angle
class Spotlight : public Lightsource {

    public:
	Spotlight(const Vector& pos, const Vector& direction, double angle, double cut_angle);
	double getIntensity(const Vector& direction_to_light, double cos) const;
	const Vector& getPosition() const { return _pos; };
        void transform(const Matrix& m);

    private:
	Vector _pos;
	Vector _dir;
	double _angle;
	double _cut_angle;
};

#endif
