
#include "camera.h"
#include "math/matrix.h"

Camera::Camera(Vector p, Vector d) {
    position = p;
    direction = d;
}

Camera::~Camera() {
}

void Camera::transform(Matrix &m) {
    position = m * position;
}

Vector & Camera::getPosition() {
    return position;
}

