
#ifndef PARSER_CAMERA_FACTORY_H
#define PARSER_CAMERA_FACTORY_H

#include <libguile.h>

/**
 * Factory for camera-related Scheme-procedures.
 */
class CameraFactory {

    public:
	static SCM make_pinhole_camera(SCM s_options);

	static void register_procs();

};

#endif
