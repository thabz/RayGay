
#ifndef RENDERER_SETTINGS
#define RENDERER_SETTINGS

/**
 * Structure for storing settings for the renderers.
 */
class RendererSettings {
    public:
	RendererSettings();

	enum RendererType {
	    PHOTON_RENDERER,
	    RAYTRACER
	};

	int global_photons_num;
	int caustic_photons_num;
	int estimate_radius;
	int estimate_samples;
	int final_gather_rays;
	double cache_tolerance;
	RendererType renderertype;
};

inline RendererSettings::RendererSettings() {
    global_photons_num = 10000;
    caustic_photons_num = 0;
    estimate_radius = 30;
    estimate_samples = 300;
    final_gather_rays = 10;
    renderertype = RAYTRACER;
    cache_tolerance = 0.1;
}

#endif
