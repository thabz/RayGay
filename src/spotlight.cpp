
#include <math.h>
#include "spotlight.h"
#include "vector.h"
#include "matrix.h"
#include "rgb.h"
#include <cassert>
#include "constants.h"
#include <iostream>

Spotlight::Spotlight(const Vector& pos, const Vector& dir, double angle, double cut_angle) {
    _pos = pos;
    _dir = dir;
    _dir.normalize();
    _angle = angle;
    _cut_angle = cut_angle;
}

void Spotlight::transform(const Matrix& m) {
    _pos = m * _pos;
    _dir = m.extractRotation() * _dir;
}

double Spotlight::getIntensity(const Vector& direction_to_light, double cos) const {
    // Angle between light-direction and direction from light to incident
    double b = (double(-1) * direction_to_light) * _dir;
    if (b >= 1.0 ) b = 1.0; // This fixes a rounding error in math.h
    double a = acos(b);  
    if (a <= _cut_angle) {
	return 1.0;
    } else if (a <= _angle) {
	return 1.0 - (a - _cut_angle) / (_angle - _cut_angle);
    } else {
	return 0.0;
    }
}

