#ifndef CAMERA_H 
#define CAMERA_H

#include "math/vector.h"

class Matrix;

/// Datavalue class for a camera which currently just is a position and a direction.
class Camera {
    public:
	/// Constructor (deprecated)
	Camera(Vector position, Vector direction);

	/// Constructor
	Camera(Vector position, Vector lookAt, Vector up, double fieldOfView);

	/// Desctructor
	~Camera();

	/// Transform position and direction 
	void transform(Matrix& m);

	/// Returns position of camera
	Vector& getPosition();

	// Enable adaptive supersampling
	void enableAdaptiveSupersampling(unsigned int depth);

	
	bool isAAEnabled() const { return aa_enabled; }; 

	unsigned int getAADepth() const { return aa_depth; };

        // Set aperture
	void setAperture(double radius);
	
	// Set focal point (default is the lookAt vector)
	void setFocalPoint(const Vector& focalPoint);
	
    private:
	Vector position;
	Vector direction;
	Vector up;
	Vector focal_point;

	double aperture;

	bool aa_enabled;
	unsigned int aa_depth;

};

inline
void Camera::setAperture(double radius) {
    this->aperture = radius;
}

inline
void Camera::setFocalPoint(const Vector& focalPoint) {
    this->focal_point = focalPoint;
}
#endif

