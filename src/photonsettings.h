
#ifndef PHOTON_SETTINGS
#define PHOTON_SETTINGS

class PhotonSettings {
    public:
	PhotonSettings();

	int photons_num;
	int estimate_radius;
	int estimate_samples;
	int final_gather_rays;
};

inline PhotonSettings::PhotonSettings() {
    photons_num = 10000;
    estimate_radius = 30;
    estimate_samples = 300;
    final_gather_rays = 10;
}

#endif
