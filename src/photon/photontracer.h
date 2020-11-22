
#ifndef PHOTON_TRACER
#define PHOTON_TRACER

#include <pthread.h>

class Scene;
class KdTree;
class Lightsource;
class Ray;
class Vector;
class QMCSequence;
class RGB;
class CausticsMap;
class GlobalPhotonMap;

/**
 * Phase one of a photonraytracer. This will trace photons
 * from the lights, bounce them through the scene and register
 * the absorbed photons in both a global- and a caustic-photonmap.
 */
class PhotonTracer {

public:
  PhotonTracer(Scene *scene, KdTree *space, GlobalPhotonMap *globalphotonmap,
               CausticsMap *photonmap);
  ~PhotonTracer();
  void trace(int threads_num);
  void trace();

private:
  void trace(const Ray &ray, RGB power, int bounces);
  void printProgress();

  pthread_mutex_t mutex_print;
  uint32_t skip_print;
  Scene *scene;
  KdTree *space;
  CausticsMap *causticsmap;
  GlobalPhotonMap *globalphotonmap;
  QMCSequence *qmcsequence;
};

#endif
