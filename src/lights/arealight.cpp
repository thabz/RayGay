
#include <cassert>

#include "lights/arealight.h"
#include "paths/circle.h"
#include "intersection.h"
#include "spacesubdivider.h"

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

    /*
    ci'rcles = new (Circle*)[num];
    ts = double[num];
    hints = new object[num];
*/
    assert(n > 1);

    for(int i = 0; i < n; i++) {
	double r = radius*(double(i)/(n-1));
	circles.push_back(new Circle(pos,r,dir));
	ts.push_back((double(rand()) / RAND_MAX));
	hints.push_back(NULL);
    }
}

Arealight::~Arealight() {
}

void Arealight::transform(const Matrix& m) {
    for(int i = 0; i < num; i++) {
	circles[i]->transform(m);
    }
}

const Vector& Arealight::getPosition() const {
    return position;
}

Vector Arealight::getPosition(int i) const {
    assert(i < num);
    double j = jitter * (double(rand()) / RAND_MAX);
    double t = (ts[i] + j) - int(ts[i] + j);
    return circles[i]->getPoint(t);
}

Lightinfo Arealight::getLightinfo(const Intersection& inter, const Vector& normal, SpaceSubdivider* space) const {
    Lightinfo info;
    Vector direction_to_light;
    info.direction_to_light = position - inter.getPoint();
    info.direction_to_light.normalize();
    info.cos = info.direction_to_light * normal;
    if (info.cos > 0.0) {
	int count = 0;
	for(int i = 0; i < num; i++) {
	    direction_to_light = getPosition(i) - inter.getPoint();
	    direction_to_light.normalize();

	    Ray ray_to_light = Ray(inter.getPoint(),direction_to_light,-1.0);
	    // TODO: Fix brug af hints[i] herunder
	    if (space->intersectForShadow(ray_to_light,hints[i])) {
		count++;
		hints[i] = NULL;
	    } else {
		hints[i] = space->getLastIntersection()->getObject();
	    }
	}
	info.intensity = double(count) / num;
    }
    return info;
}
