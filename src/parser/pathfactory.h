
#ifndef PARSER_PATH_FACTORY_H
#define PARSER_PATH_FACTORY_H

#include <libguile.h>

/**
 * Factory for path-related Scheme-procedures.
 */
class PathFactory {

    public:
	static SCM make_circle(SCM s_center, SCM s_radius, SCM s_normal);

	static SCM make_ellipse (SCM s_center, SCM s_radus_x, SCM s_radius_y, SCM s_normal);

	static SCM make_spiral(SCM s_path, SCM s_radius, SCM s_windings, SCM s_offset);

	static SCM make_bezierspline(SCM s_vector_vector);

	static SCM point_on_path(SCM s_path, SCM s_t);

	static SCM tangent_to_path(SCM s_path, SCM s_t);

	static void register_procs();
};

#endif
