#ifndef CAMERA_H 
#define CAMERA_H

#include "math/matrix.h"
#include "math/vector.h"
#include "math/vector2.h"
#include "math/halton.h"

class Ray;

/**
 * A camera implementation.
 * The camera supplies camera rays.
 */
class Camera {
    public:
	Camera(); 

	/// Constructor
	Camera(Vector position, Vector lookAt, Vector up, double fieldOfView, int width, int height);

	/// Desctructor
	virtual ~Camera();

	/// Transform the camera
	void transform(const Matrix& m);

	/// Returns position of camera
	Vector getPosition() const { return position; };
	void setPosition(const Vector& pos) { position = pos; };

	const Vector& getLookAt() const { return look_at; };
	void setLookAt(const Vector& look_at) { this->look_at = look_at; };

	Vector getDirection() const;

	const Vector& getUp() const { return up; };
	void setUp(const Vector& up) { this->up = up; };

	/// Get field of view in radians
	double getFieldOfView() const { return field_of_view_radians; };

	/// Set field of view in degrees
	void setFieldOfView(double degrees);

	/// Enable adaptive supersampling
	void enableAdaptiveSupersampling(unsigned int depth);

	/// Enable depth of field
	void enableDoF(double aperture, int samples);

	/// Says whether depth of field is enabled
	bool isDoFEnabled() const { return dof_enabled; };

	/// The numer of rays to use in oversampling depth of field
	int getDoFSamples() const { return dof_samples; };

	/// Says whether adaptive antialias is enabled
	bool isAAEnabled() const { return aa_enabled; }; 

	unsigned int getAADepth() const { return aa_depth; };

	/// Get a ray going through the screen
	virtual Ray getRay(const double x, const double y) = 0;

	void setImageSize(int width, int height) { this->width = width; this->height = height; };

	/// Project a 3D point to the 2D screen
	Vector2 project(const Vector& p) const;

    protected:
	void init();
	// General camera 
	Vector position;
	Vector up;
	Vector right;
	Vector look_at;
	double field_of_view_radians;

	double au;
	double av;
	Matrix basis;
	Matrix inv_basis;
	int width, height; ///< Image size in pixels
	bool initialized;

	// Depth of field
	bool dof_enabled;
	double dof_length;
	double dof_aperture;
	int dof_sample_count;
	int dof_samples;
	QMCSequence* dof_qmc;

	// Adaptive antialiasing
	bool aa_enabled;
	unsigned int aa_depth;
};

inline
Vector Camera::getDirection() const {
    Vector result = look_at - position;
    result.normalize();
    return result;
}

inline
void Camera::setFieldOfView(double degrees) { 
    this->field_of_view_radians = DEG2RAD(degrees); 
}

#endif

