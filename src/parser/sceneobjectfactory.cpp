
#include "parser/sceneobjectfactory.h"
#include "parser/converters.h"
#include "parser/wrapper.h"
#include "objects/sphere.h"
#include "objects/ellipsoid.h"
#include "objects/box.h"
#include "objects/cylinder.h"
#include "objects/torus.h"
#include "objects/extrusion.h"
#include "objects/heightfield.h"


SCM SceneObjectFactory::make_sphere(SCM s_center, SCM s_radius, SCM s_material) 
{
    char* proc = "make-sphere";
    Vector center = scm2vector(s_center, proc, 1);
    double radius = scm_num2double(s_radius, 2, proc);
    Material* material = scm2material(s_material, proc, 3);
    Sphere* sphere = new Sphere(center,radius,material);
    return sceneobject2scm(sphere);
}

SCM SceneObjectFactory::make_ellipsoid(SCM s_center, SCM s_radii, SCM s_material) 
{
    char* proc = "make-ellipsoid";
    Vector center = scm2vector(s_center, proc, 1);
    Vector radii = scm2vector(s_radii, proc, 2);
    Material* material = scm2material(s_material, proc, 3);
    Ellipsoid* ellipsoid = new Ellipsoid(center,radii,material);
    return sceneobject2scm(ellipsoid);
}

SCM SceneObjectFactory::make_torus(SCM s_R, SCM s_r, SCM s_material) 
{
    char* proc = "make-ellipsoid";
    double R = scm_num2double(s_R, 1, proc);
    double r = scm_num2double(s_r, 2, proc);
    Material* material = scm2material(s_material, proc, 3);
    Torus* torus = new Torus(R,r,material);
    return sceneobject2scm(torus);
}

SCM SceneObjectFactory::make_box(SCM s_corner1, SCM s_corner2, SCM s_material) 
{
    char* proc = "make-box";
    Vector corner1 = scm2vector(s_corner1, proc, 1);
    Vector corner2 = scm2vector(s_corner2, proc, 2);
    Material* material = scm2material(s_material, proc, 3);
    Box* box = new Box(corner1, corner2, material);
    return sceneobject2scm(box);
}

SCM SceneObjectFactory::make_cylinder(SCM s_begin, SCM s_end, SCM s_radius, SCM s_material) 
{
    char* proc = "make-cylinder";
    Vector begin = scm2vector(s_begin, proc, 1);
    Vector end = scm2vector(s_end, proc, 2);
    double radius = scm_num2double(s_radius, 3, proc);
    Material* material = scm2material(s_material, proc, 4);
    Cylinder* cylinder = new Cylinder(begin, end, radius, true, material);
    return sceneobject2scm(cylinder);
}

SCM SceneObjectFactory::make_extrusion(SCM s_path, SCM s_radius, SCM s_segments, SCM s_pieces, SCM s_material)
{
    char* proc = "make-extrusion";
    Path* path = scm2path(s_path, proc, 1);
    double radius = scm_num2double(s_radius, 2, proc);
    int segments = scm_num2int(s_segments, 3, proc);
    int pieces = scm_num2int(s_pieces, 4, proc);
    Material* material = scm2material(s_material, proc, 5);
    Extrusion* extrusion = new Extrusion(*path, radius, segments, pieces, material);
    return sceneobject2scm(extrusion);
}

SCM SceneObjectFactory::make_heightfield(SCM s_texture, SCM s_box, SCM s_w_div, SCM s_d_div, SCM s_material)
{
    char* proc = "make-heightfield";

    Texture* texture = scm2texture(s_texture, proc, 1);
    Vector box = scm2vector (s_box, proc, 2);
    int width_divisions = scm_num2int(s_w_div, 3, proc);
    int depth_divisions = scm_num2int(s_d_div, 4, proc);
    Material* material = scm2material(s_material, proc, 5);

    HeightField* hf = new HeightField(texture, box[1], box[0], box[2], width_divisions, depth_divisions, material);
    return sceneobject2scm(hf);
}

void SceneObjectFactory::register_procs() 
{
    scm_c_define_gsubr("make-sphere",3,0,0,
	    (SCM (*)()) SceneObjectFactory::make_sphere);
    scm_c_define_gsubr("make-ellipsoid",3,0,0,
	    (SCM (*)()) SceneObjectFactory::make_ellipsoid);
    scm_c_define_gsubr("make-box",3,0,0,
	    (SCM (*)()) SceneObjectFactory::make_box);
    scm_c_define_gsubr("make-cylinder",4,0,0,
	    (SCM (*)()) SceneObjectFactory::make_cylinder);
    scm_c_define_gsubr("make-torus",3,0,0,
	    (SCM (*)()) SceneObjectFactory::make_torus);
    scm_c_define_gsubr("make-extrusion",5,0,0,
	    (SCM (*)()) SceneObjectFactory::make_extrusion);
    scm_c_define_gsubr("make-heightfield",5,0,0,
	    (SCM (*)()) SceneObjectFactory::make_heightfield);
}

