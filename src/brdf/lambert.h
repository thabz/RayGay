
#include "brdf.h"

/**
 * The Lambert BRDF describes a perfectly diffuse surface
 * where light is scattered evenly in all directions.
 */
class Lambert : public BRDF {

  Lambert(double kd) { this->kd = kd; };

  double brdf(const Vector &wi, const Vector &wo, const Vector &normal) {
    double cosa = wi * n;
    return cosa * kd;
  }

  /**
   * Returns a cosine distributed random direction on the hemisphere
   */
  Vector pdf(const Vector &normal, const Vector &incoming) {
    return normal.randomHemisphere();
  };

private:
  double kd;
};
