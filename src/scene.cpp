#include <iostream>

#include "scene.h"
#include "object.h"
#include "lights/lightsource.h"
#include "intersection.h"
#include "ray.h"
#include "sphere.h"
#include "image/image.h"
#include "math/vector2.h"
#include "hierarchy.h"
#include "objectcollection.h"
#include "materials/materials.h"

using namespace std;

Scene::Scene() {
    hierarchy = new Hierarchy();
    environmentMap = NULL;
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

void Scene::setEnvironmentMap(const std::string& filename) {
    environmentSphere = new Sphere(Vector(0,0,0),10000,MATERIAL_SHINY_BLUE);
    environmentMap = Image::load(filename);
}

RGB Scene::getBackgroundColor(const Ray& ray) const { 
    if (environmentMap == NULL) { 
        return bg_color; 
    } else {
	Intersection i = environmentSphere->intersect(ray);
	double u,v;
	Vector2 uv = i.getObject()->getUV(i);
	u = uv[0]; v = uv[1];
        u -= int(u);
	v -= int(v);
	return environmentMap->getBiCubicTexel(u,v);
    }
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
