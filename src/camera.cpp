
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

void Camera::enableDoF(double aperture, const Vector& focalpoint, int samples) {
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

Ray Camera::getRay(const double x, const double y) {
    if (!initialized) 
	init();

    double du = -au + ((2.0 * au * x) / (width));
    double dv = -av + ((2.0 * av * y) / (height));
    Vector dir = basis * Vector(du,dv,-1);
    Vector pos = position;
    dir.normalize();

    if (dof_enabled) {
	// Jitter position and adjust direction

       Vector P = pos + dir * dof_length; // The point to aim at

       if (++dof_sample_count > dof_samples) {
	   dof_qmc->reset();
	   dof_sample_count = 0;
       }

       double* qmc = dof_qmc->getNext();
       Vector2 disc = Math::shirleyDisc(qmc[0],qmc[1]) * dof_aperture;
       Vector jitter_pos = up * disc[0] + right * disc[1];

       pos = pos + jitter_pos;
       dir = P - pos;
       dir.normalize();
    }
    
    return Ray(pos, dir, 1.0);
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
