#ifndef AREALIGHT_H
#define AREALIGHT_H

#include "lightsource.h"

class Circle;

/// Lightsource that produces soft shadows
class Arealight : public Lightsource {

    public:
	/// Constructor
	Arealight(const Vector& pos, const Vector& dir, double radius, int num, double jitter);
	~Arealight();
	Lightinfo getLightinfo(const Intersection& inter, const Vector& normal, const Scene& scene) const;
	const Vector& getPosition() const;
        void transform(const Matrix& m);

    private:
	Circle* circles;
	double* ts;
	double jitter;
	int num;
	Vector position;
	Vector getPosition(int i) const;
};

#endif
