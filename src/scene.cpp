#include <iostream>

#include "scene.h"
#include "matrix.h"
#include "object.h"
#include "lightsource.h"
#include "intersection.h"
#include "ray.h"
#include "hierarchy.h"
#include "constants.h"
#include "objectcollection.h"

using namespace std;

Scene::Scene() {
    hierarchy = new Hierarchy();
}

Scene::~Scene() {
}

void Scene::addObject(object* obj) {
    objects.push_back(obj);
}

void Scene::addObject(ObjectCollection* obj) {
    objectcollections.push_back(obj);
}

void Scene::addLight(Lightsource* light) {
    lights.push_back(light);
 //   objects.push_back(light); //TODO: Make sure lights are rotated too
}

void Scene::setCamera(Camera* cam) {
    camera = cam;
}

Camera* Scene::getCamera() const {
    return camera;
}

void Scene::transform(const Matrix &m) {
    for (vector<object*>::iterator p = objects.begin(); p != objects.end(); p++) {
	(*p)->transform(m);
    }
    for (vector<Lightsource*>::iterator p = lights.begin(); p != lights.end(); p++) {
	(*p)->transform(m);
    }
    for (vector<ObjectCollection*>::iterator p = objectcollections.begin(); p != objectcollections.end(); p++) {
	(*p)->transform(m);
    }
    // camera.transform(m)
}

std::vector<Lightsource*> Scene::getLightsources() {
    return lights;   
}

std::vector<object*> Scene::getObjects() {
    return objects;   
}

std::vector<ObjectCollection*> Scene::getObjectCollections() {
    return objectcollections;   
}

ostream & operator<<(ostream &os, const Scene &x) {
    os << "Hierarchy: " << *(x.hierarchy);

    return os;
}
