
#include "photon/causticsmap.h"
#include "stats.h"

CausticsMap::CausticsMap(int size) : PhotonMap<CausticPhoton>(size) {

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
