
#include "photon/causticsmap.h"
#include "stats.h"

/**
 * This is the constructor for the caustics photon map.
 *
 * To create the photon map it is necessary to specify the
 * maximum number of photons that will be stored.
 *
 * @param max_phot The maximum number of photons that will be stored.
 * @param max_dist Max distance to look for photons when doing an irradiance estimate
 * @param estimate_photons Number of photons to use when doing an irradiance estimate
 */
CausticsMap::CausticsMap(int size, double max_dist, int estimate_photons) : PhotonMap<CausticPhoton>(size,max_dist,estimate_photons) {

}

/**
 * Store a photon in the photon map
 * 
 * @param power photon power
 * @param pos photon position
 * @param dir photon direction
 */
void CausticsMap::store( const Vector& power, const Vector& pos, const Vector& dir)
{
    if (isFull())
	return;

    CausticPhoton photon;
    photon.setPower(power);
    photon.setPosition(pos);
    packVector(dir,&photon.theta,&photon.phi);
    storeit(photon);

    Stats::getUniqueInstance()->inc("Caustic photons stored");
}

/**
 * Using the Gaussian filter where each photon is miltiplied by
 *
 * w(d_p) = a ( 1 - \frac{1 - e^{(dp^2)/(2r^2) }}{ 1- e^{-b} }
 *
 * where
 *
 * d_p is the distance between x and the photon and a = 0.918 and
 * b = 1.953
 */
RGB CausticsMap::getFilteredIrradianceEstimate(const Vector& point, const Vector& normal) const {
    return irradiance_estimate(point,normal);
}
