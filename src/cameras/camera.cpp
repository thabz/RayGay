
#include "camera.h"
#include "ray.h"
#include "math/functions.h"

Camera::Camera() {
    aa_enabled = false;
    dof_enabled = false;
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
    dof_enabled = false;
    this->look_at = lookAt;
    this->up = up;
    this->up.normalize();
    this->position = position;
    this->field_of_view_radians = DEG2RAD(fieldOfView);
    init();
}

Camera::~Camera() {
    if (dof_enabled) 
	delete dof_qmc;
}

void Camera::init() {
    basis = Matrix::matrixOrient(position - look_at,up);
    inv_basis = basis.inverse();
    au = tan(field_of_view_radians / 2.0);
    av = (height * au) / width;
    initialized = true;

    this->up.normalize();
    Vector dir = look_at - position;
    dir.normalize();
    this->right = Vector::xProduct(dir,up);
    this->right.normalize();
}

void Camera::enableAdaptiveSupersampling(unsigned int depth) {
    aa_depth = depth;
    aa_enabled = depth == 0 ? false : true;
}

/**
 * Enables depth of field.
 * 
 * @param aperture The radius of the circle of confusion.
 * @param samples number of rays in oversampling
 */
void Camera::enableDoF(double aperture, int samples) {
    this->dof_aperture = aperture;
    this->dof_length = (position-look_at).length();
    this->dof_samples = samples;
    this->dof_enabled = true;
    this->dof_sample_count = 0;
    this->dof_qmc = new Halton(2,2);
}

void Camera::transform(const Matrix& m) {
    position = m * position;
    look_at = m * look_at;
    up = m * up;
    init();
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
