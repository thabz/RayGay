
#include "renderersettings.h"

RendererSettings* RendererSettings::unique_instance = NULL;

RendererSettings* RendererSettings::uniqueInstance() {
    if (unique_instance == NULL) {
        unique_instance = new RendererSettings();    
    }        
    return unique_instance;
}


/**
 * Constructor which insert default-settings.
 */
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
    anim_frames = 1;
    image_width = 640;
    image_height = 480;
    image_alloc_model = Allocator::AUTO;
    fast_preview = false;
}
