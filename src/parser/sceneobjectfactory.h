
#ifndef PARSER_SCENEOBJECT_FACTORY_H
#define PARSER_SCENEOBJECT_FACTORY_H

#include <libguile.h>

/**
 * Factory for sceneobject-related Scheme-procedures.
 */
class SceneObjectFactory {
    public:
	static SCM make_sphere(SCM s_center, SCM s_radius, SCM s_material);
	static SCM make_ellipsoid(SCM s_center, SCM s_radii, SCM s_material);
	static SCM make_box(SCM s_corner1, SCM s_corner2, SCM s_material);
	static SCM make_torus(SCM s_R, SCM s_r, SCM s_material);
	static SCM make_cylinder(SCM s_begin, SCM s_end, SCM s_radius, SCM s_material);
	static SCM make_extrusion(SCM s_path, SCM s_circle, SCM s_twists, SCM s_segments, SCM s_pieces, SCM s_material);
	static SCM make_heightfield(SCM s_texture, SCM s_bbox, SCM s_w_div, SCM s_d_div, SCM s_material);
	static SCM make_blob(SCM s_iso, SCM s_steps, SCM s_accuracy, SCM s_material, SCM s_atoms);

	static void register_procs();
};

#endif
