
#ifndef PARSER_CAMERA_FACTORY_H
#define PARSER_CAMERA_FACTORY_H

#include "scheme/scheme.h"

/**
 * Factory for camera-related Scheme-procedures.
 */
class CameraFactory {

    public:
	static SchemeObject* make_pinhole_camera(SchemeObject* s_options);

	static SchemeObject* make_lat_long_camera(SchemeObject* s_options);

	static SchemeObject* make_fisheye_camera(SchemeObject* s_options);

	static SchemeObject* make_whitted_adaptive_sampler(SchemeObject* s_options);

	static SchemeObject* make_boundary_adaptive_sampler(SchemeObject* s_options);

	static SchemeObject* make_uniform_jitter_sampler(SchemeObject* s_options);

	static SchemeObject* make_halton_sampler(SchemeObject* s_options);

	static void register_procs(Scheme* scheme);

};

#endif
