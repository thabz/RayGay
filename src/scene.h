#ifndef SCENE_H
#define SCENE_H

#include <vector>
#include <iosfwd>
#include "image/rgb.h"
#include "hierarchy.h"
#include "camera.h"

class Intersection;
class Ray;
class Lightsource;
class object;
class Matrix;
class Sphere;
class Image;
class ObjectCollection;

/// The collection of objects and lights.

class Scene {
    friend std::ostream & operator<< (std::ostream &os, const Scene &x);

    public:
	Scene();
	virtual ~Scene();	
	void addObject(object* obj);
	void addObject(ObjectCollection* obj);
	void addLight(Lightsource* light);
	void setCamera(Camera* camera);
	Camera* getCamera() const;

	virtual void transform(const Matrix& m);
	std::vector<Lightsource*> getLightsources();
	std::vector<object*> getObjects();
	std::vector<ObjectCollection*> getObjectCollections();
	void setBackgroundColor(const RGB& c) { bg_color = c; };
	RGB getBackgroundColor(const Ray& ray) const;
	void setEnvironmentMap(const std::string& filename);

    private:
	std::vector<Lightsource*> lights;
	std::vector<object*> objects;
	std::vector<ObjectCollection*> objectcollections;
	Image* environmentMap;
	Sphere* environmentSphere;
	Camera* camera;
	Hierarchy* hierarchy;
	RGB bg_color;
};

#endif

