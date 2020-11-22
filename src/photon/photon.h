
#ifndef PHOTON_H
#define PHOTON_H

#include "math/vector.h"

class RGB;

/**
 * This is the photon.
 * The power is not compressed so the
 * size is 28 bytes
 */
class Photon {
public:
  float pos[3]; ///< photon position
  short plane;  ///< splitting plane for kd-tree
  /// incoming direction
  unsigned char theta, phi;
  float power[3]; ///< photon power (uncompressed)

  /// Get the position of this photon
  Vector getPosition() const;
  /// Set the position of this photon
  void setPosition(const Vector &vector);

  /// Get the power
  RGB getPower() const;
  /// Set the power
  void setPower(const RGB &power);
};

inline Vector Photon::getPosition() const {
  return Vector(pos[0], pos[1], pos[2]);
}

#endif
