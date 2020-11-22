
#ifndef GLOBAL_PHOTON_MAP
#define GLOBAL_PHOTON_MAP

#include "genericphotonmap.h"

/**
 * A photon containing an irradiance estimate and a surface normal.
 */
class IrradiancePhoton : public Photon {
public:
  /// Precomputed irradiance estimate
  float irradiance_estimate[3];
  /// Surface normal
  unsigned char normal_theta, normal_phi;

  void setHasIrrandiance(bool b) { has_irradiance = b; };
  bool hasIrradiance() const { return has_irradiance; };

private:
  bool has_irradiance;
};

/**
 * The global photon map.
 */
class GlobalPhotonMap : public PhotonMap<IrradiancePhoton> {

public:
  GlobalPhotonMap(const int size, double max_dist, int estimate_photons);

  void preComputeIrradiances(const int step, int threads_num);
  RGB irradianceEstimate(const Vector &pos, const Vector &normal) const;
  RGB directIrradianceEstimate(const Vector &pos, const Vector &normal) const;

  void store(const RGB &power, const Vector &pos, const Vector &dir,
             const Vector &normal);

  void preComputeIrradiance(int photon_index);

private:
  class locatePhotonArgs {
  public:
    float smallest_dist;
    int smallest_index;
    Vector pos;
    Vector normal;
  };
  void locate_photon(locatePhotonArgs *const args, const int index) const;
};

#endif
