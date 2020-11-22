
#include "photon.h"
#include "image/rgb.h"
#include "math/vector.h"

void Photon::setPosition(const Vector &posi) {
  pos[0] = posi[0];
  pos[1] = posi[1];
  pos[2] = posi[2];
}

RGB Photon::getPower() const { return RGB(power[0], power[1], power[2]); }

void Photon::setPower(const RGB &powe) {
  power[0] = powe[0];
  power[1] = powe[1];
  power[2] = powe[2];
}
