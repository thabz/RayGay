
#include "camera.h"
#include "math/matrix.h"

Camera::Camera(Vector p, Vector d) {
    position = p;
    direction = d;
    aa_enabled = false;
}

/**
 *  Creates a camera. The vertical field of view is deduced from applying 
 *  the image width and height ration to the horizontal field of view.
 *
 * @param position The position of the eye point
 * @param lookAt Where the camera is pointed at
 * @param up The vector that defines up
 * @param fieldOfView The anglespan that the camera should scan horizontally, normally 45.
 */
Camera::Camera(Vector position, Vector lookAt, Vector up, double fieldOfView) {
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
    aa_enabled = depth == 0 ? false : true;
}

