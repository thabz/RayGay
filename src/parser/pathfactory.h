
#ifndef PARSER_PATH_FACTORY_H
#define PARSER_PATH_FACTORY_H

#include "scheme/scheme.h"

/**
 * Factory for path-related Scheme-procedures.
 */
class PathFactory {

    public:
	static SchemeObject* make_circle(SchemeObject* s_center, SchemeObject* s_radius, SchemeObject* s_normal);

	static SchemeObject* make_ellipse(SchemeObject* s_center, SchemeObject* s_radus_x, SchemeObject* s_radius_y, SchemeObject* s_normal);

        static SchemeObject* make_linesegment(SchemeObject* s_from, SchemeObject* s_to);
	static SchemeObject* make_spiral(SchemeObject* s_path, SchemeObject* s_radius, SchemeObject* s_windings, SchemeObject* s_offset);

	static SchemeObject* make_bezierspline(SchemeObject* s_vector_vector);
	static SchemeObject* make_catmullrom_spline(SchemeObject* s_vector_vector);

	static SchemeObject* point_on_path(SchemeObject* s_path, SchemeObject* s_t);

	static SchemeObject* tangent_to_path(SchemeObject* s_path, SchemeObject* s_t);

	static void register_procs(Scheme* scheme);
};

#endif
