
#ifndef CAUSTICS_MAP
#define CAUSTICS_MAP

#include "genericphotonmap.h"

/**
 * A photon used for caustics.
 */
class CausticPhoton : public Photon {

};

/**
 * The photonmap for storing caustic photons.
 *
 * These are photons that have travelled an LS+D path.
 */
class CausticsMap : public PhotonMap<CausticPhoton> {

    public:
	CausticsMap(int size);

	/// Creates a Gauss-filtered irrandiance estimate
	RGB getFilteredIrradianceEstimate(const Vector& point) const;

	void store( const Vector& power, const Vector& pos, const Vector& dir);
};

#endif

