
#include "parser/sceneobjectfactory.h"
#include "parser/converters.h"
#include "parser/wrapper.h"

#include "parser/schemeisosurface.h"
#include "objects/sphere.h"
#include "objects/ellipsoid.h"
#include "objects/box.h"
#include "objects/solidbox.h"
#include "objects/cylinder.h"
#include "objects/torus.h"
#include "objects/extrusion.h"
#include "objects/heightfield.h"
#include "objects/blob.h"
#include "objects/mesh.h"
#include "objects/bezierpatch.h"
#include "objects/csg.h"


SCM make_sphere(SCM s_center, SCM s_radius, SCM s_material) 
{
    char* proc = "make-sphere";
    Vector center = scm2vector(s_center, proc, 1);
    double radius = scm_num2double(s_radius, 2, proc);
    Material* material = scm2material(s_material, proc, 3);
    Sphere* sphere = new Sphere(center,radius,material);
    return sceneobject2scm(sphere);
}

SCM make_ellipsoid(SCM s_center, SCM s_radii, SCM s_material) 
{
    char* proc = "make-ellipsoid";
    Vector center = scm2vector(s_center, proc, 1);
    Vector radii = scm2vector(s_radii, proc, 2);
    Material* material = scm2material(s_material, proc, 3);
    Ellipsoid* ellipsoid = new Ellipsoid(center,radii,material);
    return sceneobject2scm(ellipsoid);
}

SCM make_torus(SCM s_R, SCM s_r, SCM s_material) 
{
    char* proc = "make-ellipsoid";
    double R = scm_num2double(s_R, 1, proc);
    double r = scm_num2double(s_r, 2, proc);
    Material* material = scm2material(s_material, proc, 3);
    Torus* torus = new Torus(R,r,material);
    return sceneobject2scm(torus);
}

SCM make_box(SCM s_corner1, SCM s_corner2, SCM s_material) 
{
    char* proc = "make-box";
    Vector corner1 = scm2vector(s_corner1, proc, 1);
    Vector corner2 = scm2vector(s_corner2, proc, 2);
    Material* material = scm2material(s_material, proc, 3);
    Box* box = new Box(corner1, corner2, material);
    return sceneobject2scm(box);
}

SCM make_solid_box(SCM s_corner1, SCM s_corner2, SCM s_material) 
{
    char* proc = "make-solid-box";
    Vector corner1 = scm2vector(s_corner1, proc, 1);
    Vector corner2 = scm2vector(s_corner2, proc, 2);
    Material* material = scm2material(s_material, proc, 3);
    SolidBox* box = new SolidBox(corner1, corner2, material);
    return sceneobject2scm(box);
}

SCM make_cylinder(SCM s_begin, SCM s_end, SCM s_radius, SCM s_material) 
{
    char* proc = "make-cylinder";
    Vector begin = scm2vector(s_begin, proc, 1);
    Vector end = scm2vector(s_end, proc, 2);
    double radius = scm_num2double(s_radius, 3, proc);
    Material* material = scm2material(s_material, proc, 4);
    Cylinder* cylinder = new Cylinder(begin, end, radius, true, material);
    return sceneobject2scm(cylinder);
}

/**
 * @param s_circle a path in the (x,y)-plane
 */
SCM make_extrusion(SCM s_path, SCM s_circle, SCM s_twists, SCM s_segments, SCM s_pieces, SCM s_material)
{
    char* proc = "make-extrusion";
    Path* path = scm2path(s_path, proc, 1);
    Path* circle = scm2path(s_circle,proc,2);
    double twists = scm_num2double(s_twists, 3, proc);
    int segments = scm_num2int(s_segments, 4, proc);
    int pieces = scm_num2int(s_pieces, 5, proc);
    Material* material = scm2material(s_material, proc, 6);

    Extrusion* extrusion = new Extrusion(*path, *circle, segments, pieces, twists, material);
    return sceneobject2scm(extrusion);
}

SCM make_heightfield(SCM s_texture, SCM s_box, SCM s_w_div, SCM s_d_div, SCM s_material)
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

SCM make_blob(SCM s_iso, SCM s_steps, SCM s_accuracy, SCM s_material, SCM s_atoms)
{
    char* proc = "make-blob";
    double iso = scm_num2double(s_iso, 1, proc);
    int steps = scm_num2int(s_steps, 2, proc);
    double accuracy = scm_num2double(s_accuracy, 3, proc);
    Material* material = scm2material(s_material, proc, 4);

    Blob* blob = new Blob(iso, steps, accuracy, material);

    // Add the atoms
    assert(SCM_NFALSEP (scm_list_p (s_atoms)));
    uint atoms_num = scm_num2int(scm_length(s_atoms),0,"");
    
    for(uint i = 0; i < atoms_num; i++) {
	SCM s_atom = scm_list_ref(s_atoms, scm_int2num(i));

	assert(SCM_NFALSEP (scm_list_p (s_atom)));
	uint length = scm_num2int(scm_length(s_atom),0,"");

	if (length == 3) {
	    SCM s_center = scm_list_ref(s_atom, scm_int2num(0));
	    SCM s_radius = scm_list_ref(s_atom, scm_int2num(1));
	    SCM s_weight = scm_list_ref(s_atom, scm_int2num(2));
	    Vector center = scm2vector(s_center, proc, 1);
	    double radius = scm_num2double(s_radius, 2, proc);
	    double weight = scm_num2double(s_weight, 3, proc);
	    blob->addAtom(center, radius, weight);
	} else if (length == 4) {
	    SCM s_from = scm_list_ref(s_atom, scm_int2num(0));
	    SCM s_to = scm_list_ref(s_atom, scm_int2num(1));
	    SCM s_radius = scm_list_ref(s_atom, scm_int2num(2));
	    SCM s_weight = scm_list_ref(s_atom, scm_int2num(3));
	    Vector from = scm2vector(s_from, proc, 1);
	    Vector to = scm2vector(s_to, proc, 2);
	    double radius = scm_num2double(s_radius, 3, proc);
	    double weight = scm_num2double(s_weight, 4, proc);
	    blob->addAtom(from, to, radius, weight);
	} else {
	    scm_wrong_type_arg(proc,5,s_atoms);
	}
    }

    return sceneobject2scm(blob);
}

SCM make_isosurface(SCM s_proc, SCM s_vec_lower, SCM s_vec_higher, SCM s_iso, SCM s_steps, SCM s_accuracy, SCM s_material)
{
    char* proc = "make-isosurface";

    Vector lower = scm2vector (s_vec_lower, proc, 2);
    Vector higher = scm2vector (s_vec_higher, proc, 3);
    double iso = scm_num2double(s_iso, 4, proc);
    int steps = scm_num2int(s_steps, 5, proc);
    double accuracy = scm_num2double(s_accuracy, 6, proc);
    Material* material = scm2material(s_material, proc, 7);

    BoundingBox bbox = BoundingBox(lower,higher);

    SchemeIsosurface* iso_surface  = new SchemeIsosurface(s_proc, bbox, steps, accuracy, iso, material);
    return sceneobject2scm(iso_surface);
}

SCM make_mesh(SCM s_material, SCM s_vertices, SCM s_triangles)
{
    char* proc = "make-mesh";
    uint length;

    Material* material = scm2material(s_material, proc, 1);
    Mesh* mesh = new Mesh(Mesh::MESH_PHONG, material);

    // Add the vertices
    assert(SCM_NFALSEP (scm_list_p (s_vertices)));
    length = scm_num2int(scm_length(s_vertices),0,"");
    for(uint i = 0; i < length; i++) {
	SCM s_vertex = scm_list_ref(s_vertices, scm_int2num(i));
	Vector vertex = scm2vector(s_vertex, proc, 1);
	mesh->addVertex(vertex);
    }
    cout << "Vertices: " << length << endl;

    // Add the triangles
    assert(SCM_NFALSEP (scm_list_p (s_triangles)));
    length = scm_num2int(scm_length(s_triangles),0,"");
    Vector2 uv = Vector2(0,0);
    uint v[3];
    try {
	for(uint i = 0; i < length; i++) {
	    SCM s_triangle = scm_list_ref(s_triangles, scm_int2num(i));
	    Vector triangle = scm2vector(s_triangle, proc, 2);
	    v[0] = uint(triangle[0]);
	    v[1] = uint(triangle[1]);
	    v[2] = uint(triangle[2]);
	    mesh->addTriangle(v);
	}
    } catch (Exception e) {
	scm_error(NULL, "make-mesh", e.getMessage().c_str(), SCM_UNSPECIFIED, NULL);
    }
    cout << "Faces: " << length << endl;

    return sceneobject2scm(mesh);
}

SCM make_bezierpatch(SCM s_points, SCM s_xres, SCM s_yres, SCM s_material) 
{
    char* proc = "make-bezierpatch";

    vector<Vector> points = scm2vectorlist(s_points, proc,1);
    uint xresolution = scm_num2int(s_xres, 2, proc);
    uint yresolution = scm_num2int(s_yres, 3, proc);
    Material* material = scm2material(s_material, proc, 4);

    assert(points.size() == 16);
    BezierPatch* patch = new BezierPatch(points, xresolution, yresolution, material);
    return sceneobject2scm(patch);
}

SCM make_difference(SCM s_left, SCM s_right, SCM s_material) 
{
    char* proc = "make-difference";
    Material* material;
    if (SCM_UNBNDP (s_material) || SCM_FALSEP (s_material)) {
        material = NULL;
    } else {
	material = scm2material(s_material,proc,3);
    }
    SceneObject* so_left = scm2sceneobject(s_left, proc, 1);
    SceneObject* so_right = scm2sceneobject(s_right, proc, 2);

    Solid* solid_left = dynamic_cast<Solid*>(so_left);
    Solid* solid_right = dynamic_cast<Solid*>(so_right);

    if (solid_left == NULL) scm_wrong_type_arg(proc,1,s_left);
    if (solid_right == NULL) scm_wrong_type_arg(proc,2,s_right);

    CSGDifference* difference = new CSGDifference(solid_left,solid_right,material);
    return sceneobject2scm(difference);
}

SCM make_intersection(SCM s_left, SCM s_right, SCM s_material) 
{
    char* proc = "make-intersection";
    Material* material;
    if (SCM_UNBNDP (s_material) || SCM_FALSEP (s_material)) {
        material = NULL;
    } else {
	material = scm2material(s_material,proc,3);
    }
    SceneObject* so_left = scm2sceneobject(s_left, proc, 1);
    SceneObject* so_right = scm2sceneobject(s_right, proc, 2);

    Solid* solid_left = dynamic_cast<Solid*>(so_left);
    Solid* solid_right = dynamic_cast<Solid*>(so_right);

    if (solid_left == NULL) scm_wrong_type_arg(proc,1,s_left);
    if (solid_right == NULL) scm_wrong_type_arg(proc,2,s_right);

    CSGIntersection* intersection = new CSGIntersection(solid_left,solid_right,material);
    return sceneobject2scm(intersection);
}

void SceneObjectFactory::register_procs() 
{
    scm_c_define_gsubr("make-sphere",3,0,0,
	    (SCM (*)()) make_sphere);
    scm_c_define_gsubr("make-ellipsoid",3,0,0,
	    (SCM (*)()) make_ellipsoid);
    scm_c_define_gsubr("make-box",3,0,0,
	    (SCM (*)()) make_box);
    scm_c_define_gsubr("make-solid-box",3,0,0,
	    (SCM (*)()) make_solid_box);
    scm_c_define_gsubr("make-cylinder",4,0,0,
	    (SCM (*)()) make_cylinder);
    scm_c_define_gsubr("make-torus",3,0,0,
	    (SCM (*)()) make_torus);
    scm_c_define_gsubr("make-extrusion",6,0,0,
	    (SCM (*)()) make_extrusion);
    scm_c_define_gsubr("make-heightfield",5,0,0,
	    (SCM (*)()) make_heightfield);
    scm_c_define_gsubr("make-blob",5,0,0,
	    (SCM (*)()) make_blob);
    scm_c_define_gsubr("make-isosurface",7,0,0,
	    (SCM (*)()) make_isosurface);
    scm_c_define_gsubr("make-mesh",3,0,0,
	    (SCM (*)()) make_mesh);
    scm_c_define_gsubr("make-bezierpatch",4,0,0,
	    (SCM (*)()) make_bezierpatch);
    scm_c_define_gsubr("make-difference",2,1,0,
	    (SCM (*)()) make_bezierpatch);
    scm_c_define_gsubr("make-intersection",2,1,0,
	    (SCM (*)()) make_bezierpatch);
}

