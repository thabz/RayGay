#ifndef CAMERA_H 
#define CAMERA_H

#include "vector.h"

class Matrix;
class Intersection;
class Ray;
class RGB;
class Image;

/**
 *  Datavalue class for a camera which currently just is
 *  a position and a direction.
 */
class Camera {
    public:
	/// Constructor
	Camera(Vector position, Vector direction);
	~Camera();

	/// Transform position and direction 
	void transform(Matrix& m);

	/// Returns position of camera
	Vector& getPosition();
	
    private:
	Vector position;
	Vector direction;
};

#endif

