
#ifndef RENDERER_SETTINGS
#define RENDERER_SETTINGS

/**
 * Structure for storing settings for the renderers.
 */
class RendererSettings {
    public:
	RendererSettings();

	/// The type of renderer
	enum RendererType {
	    PHOTON_RENDERER,
	    RAYTRACER,
	    PATHTRACER,
	    NONE 
	};

	/// The maximum number of global photons to store
	int global_photons_num;
	/// The maximum number of caustic photons to store
	int caustic_photons_num;
	/// The estimate radius when doing an irrandiance estimate from the photonmaps
	int estimate_radius;
	/// The maximum number of photons to consider when doing an irrandiance estimate from the photonmaps
	int estimate_samples;
	/// The number of rays to shoot when doing final gathering
	int final_gather_rays;
	/// The number of paths to shoot through each image pixel when doing path tracing
	int camera_paths;
	/// The tolerance in the photonmap cache
	double cache_tolerance;
	/// The type of renderer to use
	RendererType renderertype;
	/// The number of threads to spawn
	int threads_num;
};

inline RendererSettings::RendererSettings() {
    global_photons_num = 10000;
    caustic_photons_num = 0;
    estimate_radius = 30;
    estimate_samples = 300;
    final_gather_rays = 10;
    renderertype = RAYTRACER;
    cache_tolerance = 0.1;
    threads_num = 1;
    renderertype = NONE;
    camera_paths = 10;
}

#endif
