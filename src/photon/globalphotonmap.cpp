
#include <cassert>

#include "photon/globalphotonmap.h"
#include "stats.h"

/**
 * This is the constructor for the global photon map.
 *
 * To create the photon map it is necessary to specify the
 * maximum number of photons that will be stored.
 *
 * @param max_phot The maximum number of photons that will be stored.
 * @param max_dist Max distance to look for photons when doing an irradiance estimate
 * @param estimate_photons Number of photons to use when doing an irradiance estimate
 */
GlobalPhotonMap::GlobalPhotonMap(const int size, double max_dist, int estimate_photons ) : PhotonMap<IrradiancePhoton>(size,max_dist,estimate_photons) {

}

/**
 * Store a photon in the global photon map
 * 
 * @param power photon power
 * @param pos photon position
 * @param dir photon direction
 * @param normal surface normal
 */
void GlobalPhotonMap::store( const Vector& power, const Vector& pos, const Vector& dir, const Vector& normal)
{
    if (isFull())
	return;

    IrradiancePhoton photon;
    photon.setPower(power);
    photon.setPosition(pos);
    packVector(dir,&photon.theta,&photon.phi);
    packVector(normal,&photon.normal_theta,&photon.normal_phi);
    storeit(photon);
    Stats::getUniqueInstance()->inc("Global Photons stored");
}

/**
 * Precompute irradiance for every Mth photon.
 *
 * Iterate over all photons with step M and precompute the irradiance
 * estimate at their position by using the photonmap directly.
 *
 * Only every Mth photons is precomputed.
 *
 * @param M The step value
 */
void GlobalPhotonMap::preComputeIrradiances(const int M) {
    assert(M >= 1);
    for (int i = 0; i < stored_photons; i += M) {
	IrradiancePhoton* photon = &photons[i];
	preComputeIrradiance(photon);
	photon->flags |= 1;
    }
}

/**
 * Do irradiance estimate and store in photon
 *
 * @param photon The photon in question
 */
void GlobalPhotonMap::preComputeIrradiance(IrradiancePhoton* photon) {
    Vector pos = photon->getPosition();
    Vector normal = unpackVector(photon->normal_theta,photon->normal_phi);
    Vector irradiance = irradiance_estimate(pos,normal);
    for(int i = 0; i < 3; i++) {
	photon->irradiance_estimate[i] = irradiance[i];
    }
}

/**
 * Find the nearest precomputed estimate with a suitable normal.
 *
 * @param os The point we want an estimate for.
 * @param normal The surface normal at that point.
 */
RGB GlobalPhotonMap::irradianceEstimate(const Vector& pos, const Vector& normal) {
    double min_dist = max_dist;

    return irradiance_estimate(pos,normal);
}

/*
void locate_photon(IrradiancePhoton** best_photon, const Vector& pos, const Vector &normal, double* min_dist, const int index) const {

}
*/
