
#include "camera.h"
#include "math/matrix.h"

Camera::Camera(Vector p, Vector d) {
    position = p;
    direction = d;
    aa_enabled = false;
}

Camera::~Camera() {
}

void Camera::transform(Matrix &m) {
    position = m * position;
}

Vector & Camera::getPosition() {
    return position;
}

void Camera::enableAdaptiveSupersampling(unsigned int depth) {
    aa_depth = depth;
    aa_enabled = true;
}

