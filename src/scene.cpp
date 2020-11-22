#include <cassert>
#include <iostream>
#include <map>

#include "cameras/camera.h"
#include "image/rgba.h"
#include "image/texture.h"
#include "intersection.h"
#include "lights/lightsource.h"
#include "math/vector2.h"
#include "objects/object.h"
#include "objects/sphere.h"
#include "profiler.h"
#include "ray.h"
#include "scene.h"
#include "space/kdtree.h"
#include "stats.h"

using namespace std;

#define ENV_SPHERE_RADIUS 20000

Scene::Scene() {
  environmentMap = NULL;
  environmentSphere = NULL;
  camera = NULL;

  fog_enabled = false;
  fog_distance = 0;
  fog_color = RGBA(0.0, 0, 0, 1);

  setBackground(RGBA(0.0, 0, 0, 1));
}

Scene::~Scene() {
  // Cleanup the environment texture
  if (environmentMap != NULL) {
    delete environmentMap;
  }

  // Destroy the camera
  if (camera != NULL) {
    delete camera;
  }

  // Destroy all objects added to the scene
  for (vector<SceneObject *>::iterator p = objects.begin(); p != objects.end();
       p++) {
    delete *p;
  }
  objects.clear();

  // Destroy all light sources added to the scene
  for (vector<Lightsource *>::iterator p = lights.begin(); p != lights.end();
       p++) {
    delete *p;
  }
  lights.clear();
}

void Scene::addObject(SceneObject *obj) {
  Stats::getUniqueInstance()->inc(STATS_SCENE_OBJECTS_ADDED);
  assert(obj != NULL);
  objects.push_back(obj);
}

void Scene::addLight(Lightsource *light) {
  assert(light != NULL);
  lights.push_back(light);
}

void Scene::setCamera(Camera *cam) { camera = cam; }

Camera *Scene::getCamera() const { return camera; }

void Scene::setBackground(Texture *texture) {
  environmentSphere = new Sphere(Vector(0, 0, 0), ENV_SPHERE_RADIUS, NULL);
  environmentMap = texture;
}

RGBA Scene::getBackgroundColor(const Ray &ray) const {
  if (environmentMap == NULL) {
    return bg_color;
  } else {
    double t = environmentSphere->fastIntersect(ray);
    // TODO: Use correct probing depending on environment map type
    if (true) {
      Intersection i;
      environmentSphere->fullIntersect(ray, t, i);
      Vector2 uv = environmentSphere->getUV(i.getNormal());
      return environmentMap->getTexel(uv);
    } else {
      // A light probe
      // See http://www.debevec.org/Probes/ for math
      Vector D = ray.getPoint(t);
      D.normalize();
      double r = D.x() * D.x() + D.y() * D.y();
      if (r != 0) {
        r = acos(D.z()) / (M_PI * sqrt(r));
      }
      Vector2 uv = Vector2(D.x() * r, D.y() * r);
      uv = uv + Vector2(1, 1);
      uv = uv / 2;
      return environmentMap->getTexel(uv);
    }
  }
}

void Scene::transform(const Matrix &m) {
  for (vector<SceneObject *>::iterator p = objects.begin(); p != objects.end();
       p++) {
    (*p)->transform(m);
  }
  for (vector<Lightsource *>::iterator p = lights.begin(); p != lights.end();
       p++) {
    (*p)->transform(m);
  }
  camera->transform(m);
}

std::vector<Lightsource *> Scene::getLightsources() { return lights; }

std::vector<SceneObject *> Scene::getObjects() { return objects; }

void Scene::setFog(const RGB &color, const double distance) {
  this->fog_color = color;
  this->fog_distance = distance;
  this->fog_enabled = true;
}

void Scene::initSpace(KdTree *space) {
  Profiler *profiler = Profiler::create("Prepare objects", "Prepare scene");
  profiler->start();
  for (vector<SceneObject *>::iterator p = objects.begin(); p != objects.end();
       p++) {
    (*p)->prepare();
    (*p)->addSelf(space);
  }
  profiler->stop();

  objects.clear();

  profiler = Profiler::create("Prepare Kd-tree", "Prepare scene");
  profiler->start();
  space->prepare();
  profiler->stop();
}
