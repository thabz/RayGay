
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

