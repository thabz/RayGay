#ifndef SCENE_H
#define SCENE_H

#include <vector>
#include <iosfwd>
#include "rgb.h"
#include "hierarchy.h"

class Intersection;
class Ray;
class Lightsource;
class object;
class Matrix;

/// The collection of objects and lights.

class Scene {
    friend std::ostream & operator<< (std::ostream &os, const Scene &x);

    public:
	Scene();
	~Scene();	
	void addObject(object* obj);
	void addLight(Lightsource* light);
        virtual Intersection intersect(const Ray& ray) const;
	virtual void transform(const Matrix& m);
	std::vector<Lightsource*> getLightsources();
	void setBackgroundColor(const RGB& c) { bg_color = c; };
	RGB getBackgroundColor() { return bg_color; };
	void prepare();
  
    private:
	std::vector<Lightsource*> lights;
	std::vector<object*> objects;
	Hierarchy* hierarchy;
	RGB bg_color;
};

#endif

