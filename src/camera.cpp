
#include "camera.h"
#include "ray.h"

Camera::Camera() {
    aa_enabled = false;
    initialized = false;
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
Camera::Camera(Vector position, Vector lookAt, Vector up, double fieldOfView, int width, int height) {
    aa_enabled = false;
    this->look_at = lookAt;
    this->up = up;
    this->field_of_view_radians = DEG2RAD(fieldOfView);
    init();
}

Camera::~Camera() {
}

void Camera::init() {
    basis = Matrix::matrixOrient(position - look_at,up);
    inv_basis = basis.inverse();
    au = tan(field_of_view_radians / 2.0);
    av = (height * au) / width;
    initialized = true;
}

void Camera::enableAdaptiveSupersampling(unsigned int depth) {
    aa_depth = depth;
    aa_enabled = depth == 0 ? false : true;
}

void Camera::transform(const Matrix& m) {
    position = m * position;
    look_at = m * look_at;
    up = m * up;
    focal_point = m * focal_point;
}

Ray Camera::getRay(const double x, const double y) {
    if (!initialized) 
	init();
//    double du = -au + ((2.0 * au * x) / (width - 1.0));
//    double dv = -av + ((2.0 * av * y) / (height - 1.0));
    double du = -au + ((2.0 * au * x) / (width));
    double dv = -av + ((2.0 * av * y) / (height));
    Vector dir = basis * Vector(du,dv,-1);
    dir.normalize();
    return Ray(position, dir, 1.0);
}

/**
 * Map a 3D point onto the screen
 */
Vector2 Camera::project(const Vector& p) const {
    Vector v = inv_basis * (p - position);
   // if (v.z() > 0.0) return Vector2(-1,-1);

    Vector2 sp;
    sp[0] = (((v.x() / (-v.z())) + au) * (width - 1.0)) / (2.0 * au);
    sp[1] = (((v.y() / (-v.z())) + av) * (height - 1.0)) / (2.0 * av);
    return sp;
}
