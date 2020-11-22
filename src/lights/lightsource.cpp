
#include "lights/lightsource.h"
#include "math/constants.h"
#include "math/functions.h"
#include "math/matrix.h"
#include "ray.h"

Lightsource::Lightsource(const Vector &position) {
  this->position = position;
  this->fadeEnabled = false;
  this->power = RGB(1.0, 1.0, 1.0);
}

/**
 * Transforms this lightsource..
 *
 * @param m a transformation matrix
 */
void Lightsource::transform(const Matrix &m) { position = m * position; }

/**
 * This method sets the attenuation parameters. The attenuation \f$ A \f$
 * is a factor [0,1] which describes the intensity of light based
 * of distance from a lightsource.
 *
 * \f[ A = \frac{1}{1 + \left( \frac{d}{D} \right)^P } \f]
 *
 * Where \f$ D \f$ is fadeDistance and \f$ P \f$ is fadePower.
 *
 * @param fadeDistance Distance from light where fading begins
 * @param fadePower The fading falloff rate.
 */
void Lightsource::setAttenuation(double fadeDistance, double fadePower) {
  this->fadeDistance = fadeDistance;
  this->fadePower = fadePower;
  this->fadeEnabled = true;
}

/**
 * Get the attenuation at a point.
 * @param point well, that's the point
 */
double Lightsource::getAttenuation(const Vector &point) const {
  if (fadeEnabled) {
    double d = (this->getPosition() - point).length();
    double attenuation =
        double(2) / (double(1) + pow(d / fadeDistance, fadePower));
    return Math::clamp(attenuation);
  } else {
    return double(1);
  }
}

/**
 * Get a random ray with origin in this light
 *
 * @return a ray
 */
Ray Lightsource::getRandomPhotonRay() const {
  return Ray(getPosition(), Vector::randomUnitVector(), 0);
}

void Lightsource::getSingleLightinfo(const Intersection &inter, KdTree *space,
                                     Lightinfo *info, uint32_t depth) const {
  return getLightinfo(inter, space, info, depth);
}
