
#ifndef GLOBAL_PHOTON_MAP
#define GLOBAL_PHOTON_MAP

#include "genericphotonmap.h"

/**
 * A photon containing an irradiance estimate and a surface normal.
 */
class IrradiancePhoton : public Photon {
    public:
	/// Surface normal
	unsigned char normal_theta,normal_phi;

	
};
/**
 * The global photon map.
 */
class GlobalPhotonMap : public PhotonMap<IrradiancePhoton> {

    public:
	GlobalPhotonMap(const int size);

	void store(
		const Vector& power,          // photon power
		const Vector& pos,            // photon position
		const Vector& dir,            // photon direction
		const Vector& normal);        // surface normal
};

#endif
