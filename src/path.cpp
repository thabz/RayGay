
#include <cassert>
#include "path.h"
#include "vector.h"

void Path::getPoints(int num, Vector* out) const {
    assert(num > 1);
    assert(num > 1);
    double t;
    for (int i = 0; i < num; i++) {
	t = double(i) / (double(num-1));
	out[i] = getPoint(t);
    }

}

void Path::getTangents(int num, Vector* out) const {
    assert(num > 1);
    double t;
    for (int i = 0; i < num; i++) {
	t = double(i) / (double(num-1));
	out[i] = getTangent(t);
    }
}

bool Path::isClosed() const {
    return getPoint(0) == getPoint(1);
}
