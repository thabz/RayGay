
#include <cmath>
#include "paths/line.h"

/**
 * Constructor takes two points that the line pass
 * 
 * @param a first point
 * @param b second point
 */
Line::Line(const Vector& a, const Vector& b) {
    this->a = a;
    this->b = b;
}

double Line::sqrDistance(const Vector& p) const {
    Vector diff = p - a;
    Vector dir = getDirection();
    double sqrlen = dir.norm();
    double t = (diff * dir ) / sqrlen;
    diff = diff - t * dir;
    return diff.norm();
}

double Line::distance(const Vector& p) const {
    return sqrt(sqrDistance(p));
}

bool Line::contains(const Vector& p) const {
    return IS_ZERO(sqrDistance(p));
}

bool Line::isParallel(const Line& other) const {
    Vector thisDir = getDirection();
    thisDir.normalize();
    Vector otherDir = other.getDirection();
    otherDir.normalize();
    return thisDir == otherDir || (thisDir * -1 == otherDir);
}

Vector Line::getDirection() const {
    return b - a;
}

Vector Line::intersection(const Line& line) const {
    // TODO: Implement
    return Vector(0,0,0);
}

bool Line::operator==(const Line &other) const {
    return other.contains(a) && other.contains(b);
}
