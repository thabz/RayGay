#ifndef CAMERA_H 
#define CAMERA_H

#include "math/matrix.h"
#include "math/vector.h"

/// Datavalue class for a camera which currently just is a position and a direction.
class Camera {
    public:
	Camera(); 
	
	/// Constructor
	Camera(Vector position, Vector lookAt, Vector up, double fieldOfView);

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

	bool isAAEnabled() const { return aa_enabled; }; 

	unsigned int getAADepth() const { return aa_depth; };

	// Set aperture
	void setAperture(double radius);

	// Set focal point 
	void setFocalPoint(const Vector& focalPoint);

    private:
	// General camera 
	Vector position;
	Vector up;
	Vector look_at;
	double field_of_view_radians;

	// Depth of field
	Vector focal_point;
	double aperture;

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

inline
void Camera::setAperture(double radius) {
    this->aperture = radius;
}

inline
void Camera::setFocalPoint(const Vector& focalPoint) {
    this->focal_point = focalPoint;
}
#endif

