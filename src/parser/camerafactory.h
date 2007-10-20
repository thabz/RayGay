
#ifndef PARSER_CAMERA_FACTORY_H
#define PARSER_CAMERA_FACTORY_H

#include "scheme/scheme.h"

/**
 * Factory for camera-related Scheme-procedures.
 */
class CameraFactory {

    public:
    	static SchemeObject* s_camera_p(SchemeObject* s_obj);
	static SchemeObject* s_make_pinhole_camera(SchemeObject* s_options);
	static SchemeObject* s_make_lat_long_camera(SchemeObject* s_options);
	static SchemeObject* s_make_fisheye_camera(SchemeObject* s_options);

    	static SchemeObject* s_sampler_p(SchemeObject* s_obj);
	static SchemeObject* s_make_whitted_adaptive_sampler(SchemeObject* s_options);
	static SchemeObject* s_make_boundary_adaptive_sampler(SchemeObject* s_options);
	static SchemeObject* s_make_uniform_jitter_sampler(SchemeObject* s_options);
	static SchemeObject* s_make_halton_sampler(SchemeObject* s_options);

	static void register_procs(Scheme* scheme);

};

#endif
