#ifndef CAMERA_H 
#define CAMERA_H

#include "vector.h"
#include "scene.h"
#include <map>

class Matrix;
class Intersection;
class Ray;
class RGB;
class Image;


class Camera {
    public:
	Camera(Vector position, Vector direction, Scene& sc);

	~Camera();

	void transform(Matrix& m);
	Vector& getPosition();
	void render(Image*);
	
    private:
	Scene scene;
	Vector position;
	Vector direction;
	RGB shade(const Ray&, Intersection&, int depth);
	RGB trace(const Ray&, int depth);
	RGB getPixel(double x, double y);
};

#endif

