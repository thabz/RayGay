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
	~Camera();

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
	void enableDoF(double aperture, const Vector& focalpoint, int samples);
	bool isDoFEnabled() const { return dof_enabled; };
	int getDoFSamples() const { return dof_samples; };

	bool isAAEnabled() const { return aa_enabled; }; 

	unsigned int getAADepth() const { return aa_depth; };

	Ray getRay(const double x, const double y);

	void setImageSize(int width, int height) { this->width = width; this->height = height; };

	Vector2 project(const Vector& p) const;

    private:
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

