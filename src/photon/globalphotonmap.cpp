
#include <cassert>

#include "globalphotonmap.h"

GlobalPhotonMap::GlobalPhotonMap(const int size) : PhotonMap<IrradiancePhoton>(size) {

}

/**
 * Store a photon in the photon map
 * 
 * @param power photon power
 * @param pos photon position
 * @param dir photon direction
 * @param normal surface normal
 */
void GlobalPhotonMap::store( const Vector& power, const Vector& pos, const Vector& dir, const Vector& normal)
{
    IrradiancePhoton photon;
    photon.setPower(power);
    photon.setPosition(pos);
    packVector(dir,&photon.theta,&photon.phi);
    packVector(normal,&photon.normal_theta,&photon.normal_phi);
    storeit(photon);
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

void GlobalPhotonMap::preComputeIrradiance(IrradiancePhoton* photon) {
    // TODO: Do irradiance estimate and store in photon
}

/**
 * Find the nearest precomputed estimate with a suitable normal.
 *
 * @param os The point we want an estimate for.
 * @param normal The surface normal at that point.
 */
RGB GlobalPhotonMap::irradianceEstimate(const Vector& pos, const Vector& normal) {

}

