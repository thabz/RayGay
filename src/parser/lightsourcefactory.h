
#ifndef PARSER_LIGHTSOURCE_FACTORY_H
#define PARSER_LIGHTSOURCE_FACTORY_H

#include <libguile.h>

/**
 * Factory for lightsource-related Scheme-procedures.
 */
class LightsourceFactory {
    public:
	static SCM make_pointlight(SCM pos);
	static SCM make_arealight(SCM pos, SCM dir, SCM radius, SCM num, SCM jitter);
	static SCM make_spotlight(SCM pos, SCM lookat, SCM angle, SCM cut_angle);
	static SCM make_skylight(SCM radius, SCM num);
	static void register_procs();
};

#endif
