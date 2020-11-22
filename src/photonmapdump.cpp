
#include "photonmapdump.h"
#include "cameras/camera.h"
#include "image/image.h"
#include "photon/globalphotonmap.h"
#include "scene.h"

void PhotonMapDump::render(Scene *scene, Image *image,
                           GlobalPhotonMap *photonmap, int num) {

  Camera *camera = scene->getCamera();
  Photon *photons = photonmap->list();
  for (int i = 0; i < num; i++) {
    Photon photon = photons[i];
    Vector p3 = Vector(photon.pos[0], photon.pos[1], photon.pos[2]);
    RGB intensity = RGB(photon.power[0], photon.power[0], photon.power[0]);
    Vector2 p2 = camera->project(p3);
    if (p2[0] != -1) {
      image->setRGBA(p2, intensity);
    }
  }
}
