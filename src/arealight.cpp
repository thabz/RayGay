
#include "arealight.h"
#include <cassert>
#include <stdlib.h>
#include "constants.h"
#include "circle.h"
#include "intersection.h"
#include "scene.h"

/**
 * Constructs an area light
 *
 * @param pos Position
 * @param dir Direction
 * @param radius The radius
 * @param num Number of samples
 * @param jitter How many percent the light can jitter with value in [0,1]
 */
Arealight::Arealight(const Vector& pos, const Vector& dir, double radius, int n, double j) {
    num = n;
    jitter = j;
    position = pos;

    circles = new Circle[num];
    ts = new double[num];

    assert(n > 1);

    for(int i = 0; i < n; i++) {
	double r = radius*(double(i)/(n-1));
	circles[i] = Circle(pos,r,dir);
	ts[i] = (double(rand()) / RAND_MAX);
    }
}

Arealight::~Arealight() {
    delete [] circles;
    delete [] ts;
}

void Arealight::transform(const Matrix& m) {
    for(int i = 0; i < num; i++) {
	circles[i].transform(m);
    }
}

const Vector& Arealight::getPosition() const {
    return position;
}

Vector Arealight::getPosition(int i) const {
    assert(i < num);
    double j = jitter * (double(rand()) / RAND_MAX);
    double t = (ts[i] + j) - int(ts[i] + j);
    return circles[i].getPoint(t);
}

Lightinfo Arealight::getLightinfo(const Intersection& inter, const Vector& normal, const Scene& scene) const {
    Lightinfo info;
    Vector direction_to_light;
    info.direction_to_light = position - inter.point;
    info.direction_to_light.normalize();
    info.cos = info.direction_to_light * normal;
    if (info.cos > 0.0) {
	int count = 0;
	for(int i = 0; i < num; i++) {
	    direction_to_light = getPosition(i) - inter.point;
	    direction_to_light.normalize();

	    Ray ray_to_light = Ray(inter.point,direction_to_light,-1.0);
	    Intersection i2 = scene.intersect(ray_to_light);
	    if (!i2.intersected)
		count++;
	}
	info.intensity = double(count) / num;
    }
    return info;

}
