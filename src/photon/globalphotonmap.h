
#ifndef GLOBAL_PHOTON_MAP
#define GLOBAL_PHOTON_MAP

#include "genericphotonmap.h"

/**
 * A photon containing an irradiance estimate and a surface normal.
 */
class IrradiancePhoton : public Photon {
    public:
	/// Irrandiance estimate
	float irra_estimate[3];
	/// Surface normal
	unsigned char normal_theta,normal_phi;
	/// Flags
	unsigned int flags;
};

/**
 * The global photon map.
 */
class GlobalPhotonMap : public PhotonMap<IrradiancePhoton> {

    public:
	GlobalPhotonMap(const int size);

	void preComputeIrradiances(const int step);
	RGB irradianceEstimate(const Vector& pos, const Vector& normal);

	void store(
		const Vector& power,          // photon power
		const Vector& pos,            // photon position
		const Vector& dir,            // photon direction
		const Vector& normal);        // surface normal
	
    private:
	void preComputeIrradiance(IrradiancePhoton* photon);
};

#endif
