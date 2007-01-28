
#ifndef RENDERER_SETTINGS
#define RENDERER_SETTINGS

#include "allocator.h"

/**
 * Structure for storing settings for the renderers.
 */
class RendererSettings 
{
    public:
        static RendererSettings* uniqueInstance();
   
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
	double estimate_radius;
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
	/// Frames in animation
	int anim_frames;
	/// Width of output image
	int image_width;
	/// Height of output image
	int image_height;
	/// Storage model for images (in-memory or mmap'ed)
	Allocator::model_t image_alloc_model;
	/// Choose faster alternatives everywhere
	bool fast_preview;
    private:	
    	RendererSettings();
        static RendererSettings* unique_instance;
};


#endif
