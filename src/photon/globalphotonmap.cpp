
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
    photon.setHasIrrandiance(false);
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
    photon->setHasIrrandiance(true);
}

/**
 * Find the nearest precomputed estimate with a suitable normal.
 *
 * @param os The point we want an estimate for.
 * @param normal The surface normal at that point.
 */
RGB GlobalPhotonMap::directIrradianceEstimate(const Vector& pos, const Vector& normal) const {
    return irradiance_estimate(pos,normal);
}

RGB GlobalPhotonMap::irradianceEstimate(const Vector& pos, const Vector& normal) const {

    locatePhotonArgs args;
    args.smallest_dist = 100;//(max_dist*max_dist) / 4.0;
    args.smallest_index = -1;
    args.pos = pos;
    args.normal = normal;

    locate_photon(&args,1);

    if (args.smallest_index == -1)
	return RGB(0.0,0.0,0.0);

    const IrradiancePhoton* p = &photons[args.smallest_index];
    RGB irradiance;
    for(int i = 0; i < 3; i++) {
	 irradiance[i] = p->irradiance_estimate[i];
    }
    return irradiance;
}

void GlobalPhotonMap::locate_photon(locatePhotonArgs* const args, const int index) const {
    const IrradiancePhoton* p = &photons[index];
    float dist1;

    if (index < half_stored_photons) {
	// Maybe recurse then
	dist1 = args->pos[p->plane] - p->pos[p->plane];
	if (dist1 > 0.0) { // if dist_to_plane is positive search right plane
	    locate_photon(args,2*index+1);
	    if (dist1*dist1 < args->smallest_dist) {
		locate_photon(args,2*index);
	    }
	} else { // dist_to_plane is negative so search left first
	    locate_photon(args,2*index);
	    if (dist1*dist1 < args->smallest_dist) {
		locate_photon(args,2*index+1);
	    }
	}
    }

    if (p->hasIrradiance()) {
	// compute squared distance between current photon and args->pos

	//float dist2 = (p->getPosition() - args->pos).norm();
	dist1 = p->pos[0] - args->pos[0];
	float dist2 = dist1*dist1;
	dist1 = p->pos[1] - args->pos[1];
	dist2 += dist1*dist1;
	dist1 = p->pos[2] - args->pos[2];
	dist2 += dist1*dist1;

	if (dist2 < args->smallest_dist) {
	    //    Vector n = unpackVector(p->normal_theta,p->normal_phi);
	    //   if ((n * args->normal) > 0.9) {
	    args->smallest_dist = dist2;
	    args->smallest_index = index;
	    //  }
	}
    }
}

