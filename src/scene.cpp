#include <iostream>

#include "scene.h"
#include "matrix.h"
#include "object.h"
#include "lightsource.h"
#include "intersection.h"
#include "ray.h"
#include "hierarchy.h"
#include "constants.h"

using namespace std;

Scene::Scene() {
    hierarchy = new Hierarchy(BoundingBox(Vector(-HUGE_DOUBLE,-HUGE_DOUBLE,-HUGE_DOUBLE),Vector(HUGE_DOUBLE,HUGE_DOUBLE,HUGE_DOUBLE)));
    //hierarchy = new Hierarchy(BoundingBox(Vector(-10000,-10000,-10000),Vector(10000,10000,10000)));

}

Scene::~Scene() {
    delete hierarchy;
}

void Scene::prepare() {
    for (vector<object*>::iterator p = objects.begin(); p != objects.end(); p++) {
	hierarchy->addObject(*p);
    }
    hierarchy->optimize();
}

void Scene::addObject(object* obj) {
    objects.push_back(obj);
}

void Scene::addLight(Lightsource* light) {
    lights.push_back(light);
 //   objects.push_back(light); //TODO: Make sure lights are rotated too
}

void Scene::transform(const Matrix &m) {
    for (vector<object*>::iterator p = objects.begin(); p != objects.end(); p++) {
	(*p)->transform(m);
    }
    for (vector<Lightsource*>::iterator p = lights.begin(); p != lights.end(); p++) {
	(*p)->transform(m);
    }
}

std::vector<Lightsource*> Scene::getLightsources() {
    return lights;   
}

Intersection Scene::intersect(const Ray& ray) {
    return hierarchy->intersect(ray);
}

ostream & operator<<(ostream &os, const Scene &x) {
    os << "Hierarchy: " << *(x.hierarchy);

    return os;
}
