
#include "photon/causticsmap.h"
#include "stats.h"

/**
 * This is the constructor for the caustics photon map.
 *
 * To create the photon map it is necessary to specify the
 * maximum number of photons that will be stored.
 *
 * @param max_phot The maximum number of photons that will be stored.
 * @param max_dist Max distance to look for photons when doing an irradiance
 * estimate
 * @param estimate_photons Number of photons to use when doing an irradiance
 * estimate
 */
CausticsMap::CausticsMap(int size, double max_dist, int estimate_photons)
    : PhotonMap<CausticPhoton>(size, max_dist, estimate_photons) {}

/**
 * Store a photon in the photon map
 *
 * @param power photon power
 * @param pos photon position
 * @param dir photon direction
 */
void CausticsMap::store(const RGB &power, const Vector &pos,
                        const Vector &dir) {
  if (isFull())
    return;

  CausticPhoton photon;
  photon.setPower(power);
  photon.setPosition(pos);
  packVector(dir, &photon.theta, &photon.phi);
  storeit(photon);

  Stats::getUniqueInstance()->inc(STATS_CAUSTIC_PHOTONS_STORED);
}

/**
 * Get a filtered irradiance estimate.
 *
 * Using the Gaussian filter with weights
 *
 * \f[ w(d_p) = \alpha \left( 1 - \frac{1 - e^{-\beta \frac{d_p^2}{2r^2} }}{ 1-
 * e^{-\beta} } \right) \f]
 *
 * where \f$ d_p \f$ is the distance between point and the photon and \f$ \alpha
 * \f$ = 0.918 and \f$ \beta \f$ = 1.953.
 *
 * This filter is normalized so the weights are just multiplied to the
 * irradiance value for each photon.
 */
RGB CausticsMap::getFilteredIrradianceEstimate(const Vector &point,
                                               const Vector &normal) const {
  // TODO: Implement by modifying irradiance_estimate(...) code
  return irradiance_estimate(point, normal);
}
