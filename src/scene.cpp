#include <iostream>
#include <map>

#include "scene.h"
#include "camera.h"
#include "lights/lightsource.h"
#include "intersection.h"
#include "ray.h"
#include "image/image.h"
#include "image/rgba.h"
#include "math/vector2.h"
#include "objects/objectcollection.h"
#include "objects/sphere.h"
#include "objects/object.h"
#include "stats.h"
#include "space/spacesubdivider.h"

using namespace std;

Scene::Scene() {
    environmentMap = NULL;
    fog_enabled = false;
    setBackgroundColor(RGBA(0.0,0,0,1));
}

Scene::~Scene() {
}

void Scene::addObject(SceneObject* obj) {
    Stats::getUniqueInstance()->inc("SCENE: Objects added");
    objects.push_back(obj);
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
    environmentSphere = new Sphere(Vector(0,0,0),10000,NULL);
    environmentMap = Image::load(filename);
}

RGBA Scene::getBackgroundColor(const Ray& ray) const { 
    if (environmentMap == NULL) { 
        return bg_color; 
    } else {
	// TODO: Optimize pushing a *i on stack below...
	environmentSphere->intersect(ray);
	Intersection* i = environmentSphere->getLastIntersection();
	double u,v;
	Vector2 uv = i->getObject()->getUV(*i);
	u = uv[0]; v = uv[1];
        u -= int(u);
	v -= int(v);
	return environmentMap->getBiCubicTexel(u,v);
    }
}

void Scene::transform(const Matrix &m) {
    for (vector<SceneObject*>::iterator p = objects.begin(); p != objects.end(); p++) {
	(*p)->transform(m);
    }
    for (vector<Lightsource*>::iterator p = lights.begin(); p != lights.end(); p++) {
	(*p)->transform(m);
    }
    camera->transform(m);
}

std::vector<Lightsource*> Scene::getLightsources() {
    return lights;   
}

std::vector<SceneObject*> Scene::getObjects() {
    return objects;   
}

void Scene::setFog(const RGB& color, const double distance) {
    this->fog_color = color;
    this->fog_distance = distance;
    this->fog_enabled = true;
}

void Scene::initSpace(SpaceSubdivider* space) {
    Stats::getUniqueInstance()->beginTimer("Preparing space");
    for (vector<SceneObject*>::iterator p = objects.begin(); p != objects.end(); p++) {
	(*p)->prepare();
	(*p)->addSelf(space);
    }

    space->prepare();
    Stats::getUniqueInstance()->endTimer("Preparing space");
}
