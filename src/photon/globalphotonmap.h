
#ifndef GLOBAL_PHOTON_MAP
#define GLOBAL_PHOTON_MAP

#include "genericphotonmap.h"

/**
 * A photon containing an irradiance estimate and a surface normal.
 */
class IrradiancePhoton : public Photon {
    public:

	/// Precalculate the irradiance
	void preCalculateIrradiances();

	/// Get the surface normal of this photon
	Vector getNormal() const;
	/// Set surface normal of this photon
	void setNormal(const Vector& vector);

    private:
	/// Surface normal
	unsigned char normal_theta,phi_theta;

	
};
/**
 * The global photon map.
 */
class GlobalPhotonMap : public PhotonMap<IrradiancePhoton> {

    public:
	GlobalPhotonMap(const int size);
};

#endif
