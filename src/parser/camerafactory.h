
#ifndef PARSER_CAMERA_FACTORY_H
#define PARSER_CAMERA_FACTORY_H

#include <libguile.h>

/**
 * Factory for camera-related Scheme-procedures.
 */
class CameraFactory {

    public:
	static SCM make_pinhole_camera(SCM s_options);

	static SCM make_lat_long_camera(SCM s_options);

	static SCM make_whitted_adaptive_sampler(SCM s_options);

	static SCM make_uniform_jitter_sampler(SCM s_options);

	static SCM make_halton_sampler(SCM s_options);

	static void register_procs();

};

#endif
