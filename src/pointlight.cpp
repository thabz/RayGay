
#include "pointlight.h"
#include "vector.h"
#include "matrix.h"
#include "rgb.h"

Pointlight::Pointlight(const Vector& pos) {
    position = pos;
}

void Pointlight::transform(const Matrix& m) {
    position = m * position;
}

double Pointlight::getIntensity(const Vector& direction_to_light, double cos) const {
    return 1.0;

}

RGB Pointlight::getDiffuseColor(const Vector& p) {
    return RGB(0.0,0.0,0.0);
};

