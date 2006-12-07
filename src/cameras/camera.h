#ifndef CAMERAS_CAMERA_H 
#define CAMERAS_CAMERA_H

#include "math/matrix.h"
#include "math/vector.h"
#include "math/vector2.h"
#include "math/halton.h"
#include "samplers/sampler.h"

class Ray;

/**
 * The abstract superclass of all cameras.
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
	void enableAdaptiveSupersampling(uint32_t depth);

	/// Enable depth of field
	void enableDoF(double aperture, int samples, const Vector& focalpoint);

	/// Says whether depth of field is enabled
	bool isDoFEnabled() const { return dof_enabled; };

	/// The numer of rays to use in oversampling depth of field
	int getDoFSamples() const { return dof_samples; };

	/// Says whether adaptive antialias is enabled
	bool isAAEnabled() const { return aa_enabled; }; 

	uint32_t getAADepth() const { return aa_depth; };

	/// Get a ray going through the screen
	Ray getRay(double x, double y);

        /// Set aspect ratio ie. image height / width
	void setAspectRatio(double aspect_ratio) { this->aspect_ratio = aspect_ratio; };

        void setZoom(const Vector2& pos, double width);

	/// Project a 3D point to the 2D screen
	Vector2 project(const Vector& p) const;

        SamplerFactory* getSamplerFactory() {
	    return sampler_factory;
	}

	void setSamplerFactory(SamplerFactory* sampler_factory) {
	    this->sampler_factory = sampler_factory;
	}
	
	void resetQMC();
	
    private:
	void init();
	SamplerFactory* sampler_factory;
	QMCSequence* get_dof_qmc();
	pthread_key_t dof_qmc_key;
	double aspect_ratio;  ///< height / width in output image
	Vector2 zoom_pos;
	double zoom_width;
	bool zoom_enabled;

    protected:
	/// Get a ray going through the screen
	virtual Ray _getRay(const double x, const double y) = 0;

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
	bool initialized;

	// Depth of field
	bool dof_enabled;
	double dof_length;
	double dof_aperture;
	int dof_sample_count;
	int dof_samples;

	// Adaptive antialiasing
	bool aa_enabled;
	uint32_t aa_depth;
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

