
#include "camera.h"

#include <iostream>
#include <math.h>

#include "vector.h"
#include "ray.h"
#include "intersection.h"
#include "object.h"
#include "rgb.h"
#include "matrix.h"
#include "scene.h"
#include "image.h"
#include "lightsource.h"


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

