
#ifndef RENDERER_SETTINGS
#define RENDERER_SETTINGS

class RendererSettings {
    public:
	RendererSettings();

	enum RendererType {
	    PHOTON_RENDERER,
	    RAYTRACER
	};

	int photons_num;
	int estimate_radius;
	int estimate_samples;
	int final_gather_rays;
	RendererType renderertype;
};

inline RendererSettings::RendererSettings() {
    photons_num = 10000;
    estimate_radius = 30;
    estimate_samples = 300;
    final_gather_rays = 10;
    renderertype = RAYTRACER;
}

#endif
