#ifndef CAMERA_H 
#define CAMERA_H

#include "vector.h"

class Matrix;
class Intersection;
class Ray;
class RGB;
class Image;


class Camera {
    public:
	Camera(Vector position, Vector direction);
	~Camera();

	void transform(Matrix& m);
	Vector& getPosition();
	
    private:
	Vector position;
	Vector direction;
};

#endif

