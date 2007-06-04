
#include "parser/sceneobjectfactory.h"
#include "parser/converters.h"
#include "parser/wrapper.h"

#include "parser/schemeisosurface.h"
#include "parser/schemeparametrizedsurface.h"
#include "objects/sphere.h"
#include "objects/ellipsoid.h"
#include "objects/box.h"
#include "objects/solidbox.h"
#include "objects/cylinder.h"
#include "objects/cone.h"
#include "objects/torus.h"
#include "objects/extrusion.h"
#include "objects/heightfield.h"
#include "objects/blob.h"
#include "objects/mesh.h"
#include "objects/ply.h"
#include "objects/bezierpatch.h"
#include "objects/csg.h"
#include "objects/julia.h"
#include "objects/marchingcubes.h"
#include "objects/bound.h"
#include "objects/transformedinstance.h"


SCM make_sphere(SCM s_center, SCM s_radius, SCM s_material) 
{
    char* proc = "make-sphere";
    Vector center = scm2vector(s_center, proc, 1);
    double radius = s_scm2double(s_radius, 2, proc);
    Material* material;
    if (s_material == S_UNSPECIFIED) {
        material = NULL;
    } else {
	material = scm2material(s_material,proc,3);
    }
    Sphere* sphere = new Sphere(center,radius,material);
    return sceneobject2scm(sphere);
}

SCM make_ellipsoid(SCM s_center, SCM s_radii, SCM s_material) 
{
    char* proc = "make-ellipsoid";
    Vector center = scm2vector(s_center, proc, 1);
    Vector radii = scm2vector(s_radii, proc, 2);
    Material* material;
    if (s_material == S_UNSPECIFIED) {
        material = NULL;
    } else {
	material = scm2material(s_material,proc,3);
    }
    Ellipsoid* ellipsoid = new Ellipsoid(center,radii,material);
    return sceneobject2scm(ellipsoid);
}

SCM make_torus(SCM s_R, SCM s_r, SCM s_material) 
{
    char* proc = "make-ellipsoid";
    double R = s_scm2double(s_R, 1, proc);
    double r = s_scm2double(s_r, 2, proc);
    Material* material;
    if (s_material == S_UNSPECIFIED) {
        material = NULL;
    } else {
	material = scm2material(s_material,proc,3);
    }
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
    Material* material;
    if (s_material == S_UNSPECIFIED) {
        material = NULL;
    } else {
	material = scm2material(s_material,proc,3);
    }
    SolidBox* box = new SolidBox(corner1, corner2, material);
    return sceneobject2scm(box);
}

SCM make_cylinder(SCM s_begin, SCM s_end, SCM s_radius, SCM s_material) 
{
    char* proc = "make-cylinder";
    Vector begin = scm2vector(s_begin, proc, 1);
    Vector end = scm2vector(s_end, proc, 2);
    double radius = s_scm2double(s_radius, 3, proc);
    Material* material;
    if (s_material == S_UNSPECIFIED) {
        material = NULL;
    } else {
	material = scm2material(s_material,proc,4);
    }
    Cylinder* cylinder = new Cylinder(begin, end, radius, true, material);
    return sceneobject2scm(cylinder);
}

SCM make_uncapped_cylinder(SCM s_begin, SCM s_end, SCM s_radius, SCM s_material) 
{
    char* proc = "make-uncapped-cylinder";
    Vector begin = scm2vector(s_begin, proc, 1);
    Vector end = scm2vector(s_end, proc, 2);
    double radius = s_scm2double(s_radius, 3, proc);
    Material* material;
    if (s_material == S_UNSPECIFIED) {
        material = NULL;
    } else {
	material = scm2material(s_material,proc,4);
    }
    Cylinder* cylinder = new Cylinder(begin, end, radius, false, material);
    return sceneobject2scm(cylinder);
}

SCM make_cone(SCM s_begin, SCM s_end, SCM s_radius_begin, SCM s_radius_end, SCM s_material) 
{
    char* proc = "make-cone";
    Vector begin = scm2vector(s_begin, proc, 1);
    Vector end = scm2vector(s_end, proc, 2);
    double radius_begin = s_scm2double(s_radius_begin, 3, proc);
    double radius_end = s_scm2double(s_radius_end, 4, proc);
    Material* material;
    if (s_material == S_UNSPECIFIED) {
        material = NULL;
    } else {
	material = scm2material(s_material,proc,5);
    }
    Cone* cone = new Cone(begin, end, radius_begin, radius_end, true, material);
    return sceneobject2scm(cone);
}

SCM make_uncapped_cone(SCM s_begin, SCM s_end, SCM s_radius_begin, SCM s_radius_end, SCM s_material) 
{
    char* proc = "make-uncapped-cone";
    Vector begin = scm2vector(s_begin, proc, 1);
    Vector end = scm2vector(s_end, proc, 2);
    double radius_begin = s_scm2double(s_radius_begin, 3, proc);
    double radius_end = s_scm2double(s_radius_end, 4, proc);
    Material* material;
    if (s_material == S_UNSPECIFIED) {
        material = NULL;
    } else {
	material = scm2material(s_material,proc,5);
    }
    Cone* cone = new Cone(begin, end, radius_begin, radius_end, false, material);
    return sceneobject2scm(cone);
}

/**
 * @param s_circle a path in the (x,y)-plane
 */
SCM make_extrusion(SCM s_path, SCM s_circle, SCM s_twists, SCM s_segments, SCM s_pieces, SCM s_material)
{
    char* proc = "make-extrusion";
    Path* path = scm2path(s_path, proc, 1);
    Path* circle = scm2path(s_circle,proc,2);
    double twists = s_scm2double(s_twists, 3, proc);
    int segments = scm2int(s_segments, 4, proc);
    int pieces = scm2int(s_pieces, 5, proc);
    Material* material = scm2material(s_material, proc, 6);

    Extrusion* extrusion = new Extrusion(*path, *circle, segments, pieces, twists, material);
    return sceneobject2scm(extrusion);
}

SCM make_heightfield(SCM s_texture, SCM s_box, SCM s_w_div, SCM s_d_div, SCM s_material)
{
    char* proc = "make-heightfield";

    Texture* texture = scm2texture(s_texture, proc, 1);
    Vector box = scm2vector (s_box, proc, 2);
    int width_divisions = scm2int(s_w_div, 3, proc);
    int depth_divisions = scm2int(s_d_div, 4, proc);
    Material* material = scm2material(s_material, proc, 5);

    HeightField* hf = new HeightField(texture, box[1], box[0], box[2], width_divisions, depth_divisions, material);
    return sceneobject2scm(hf);
}

SCM make_blob(SCM s_iso, SCM s_steps, SCM s_accuracy, SCM s_material, SCM s_atoms)
{
    char* proc = "make-blob";
    double iso = s_scm2double(s_iso, 1, proc);
    int steps = scm2int(s_steps, 2, proc);
    double accuracy = s_scm2double(s_accuracy, 3, proc);
    Material* material = scm2material(s_material, proc, 4);

    Blob* blob = new Blob(iso, steps, accuracy, material);

    // Add the atoms
    assert(scm2bool(s_list_p (s_atoms)));
    uint32_t atoms_num = scm2int(s_length(s_atoms),0,"");
    
    for(uint32_t i = 0; i < atoms_num; i++) {
	SCM s_atom = s_list_ref(s_atoms, int2scm(i));

	assert(scm2bool(s_list_p (s_atom)));
	uint32_t length = scm2int(s_length(s_atom),0,"");

	if (length == 3) {
	    SCM s_center = s_list_ref(s_atom, int2scm(0));
	    SCM s_radius = s_list_ref(s_atom, int2scm(1));
	    SCM s_weight = s_list_ref(s_atom, int2scm(2));
	    Vector center = scm2vector(s_center, proc, 1);
	    double radius = s_scm2double(s_radius, 2, proc);
	    double weight = s_scm2double(s_weight, 3, proc);
	    blob->addAtom(center, radius, weight);
	} else if (length == 4) {
	    SCM s_from = s_list_ref(s_atom, int2scm(0));
	    SCM s_to = s_list_ref(s_atom, int2scm(1));
	    SCM s_radius = s_list_ref(s_atom, int2scm(2));
	    SCM s_weight = s_list_ref(s_atom, int2scm(3));
	    Vector from = scm2vector(s_from, proc, 1);
	    Vector to = scm2vector(s_to, proc, 2);
	    double radius = s_scm2double(s_radius, 3, proc);
	    double weight = s_scm2double(s_weight, 4, proc);
	    blob->addAtom(from, to, radius, weight);
	} else {
	    wrong_type_arg(proc,5,s_atoms);
	}
    }

    return sceneobject2scm(blob);
}

SCM make_isosurface(SCM s_proc, SCM s_vec_lower, SCM s_vec_higher, SCM s_iso, SCM s_steps, SCM s_accuracy, SCM s_material)
{
    char* proc = "make-isosurface";

    Vector lower = scm2vector (s_vec_lower, proc, 2);
    Vector higher = scm2vector (s_vec_higher, proc, 3);
    double iso = s_scm2double(s_iso, 4, proc);
    int steps = scm2int(s_steps, 5, proc);
    double accuracy = s_scm2double(s_accuracy, 6, proc);
    Material* material = scm2material(s_material, proc, 7);

    AABox bbox = AABox(lower,higher);

    SchemeIsosurface* iso_surface  = new SchemeIsosurface(s_proc, bbox, steps, accuracy, iso, material);
    return sceneobject2scm(iso_surface);
}

SCM make_parametrized_surface(SCM s_proc, SCM s_u_res, SCM s_v_res, SCM s_u_close, SCM s_v_close, SCM s_material)
{
    char* proc = "make-parametrized-surface";

    uint32_t uRes = scm2int(s_u_res, 2, proc);
    uint32_t vRes = scm2int(s_v_res, 3, proc);
    bool uClose = scm2bool(s_u_close);
    bool vClose = scm2bool(s_v_close);
    Material* material = scm2material(s_material, proc, 6);

    SchemeParametrizedSurface* surface  = new SchemeParametrizedSurface(s_proc, uRes, vRes, uClose, vClose, material);
    return sceneobject2scm(surface);
}

SCM make_mesh(SCM s_material, SCM s_vertices, SCM s_triangles)
{
    char* proc = "make-mesh";
    uint32_t length;

    Material* material = scm2material(s_material, proc, 1);
    Mesh* mesh = new Mesh(Mesh::MESH_PHONG, material);

    // Add the vertices
    assert(scm2bool(s_list_p (s_vertices)));
    length = scm2int(s_length(s_vertices),0,"");
    for(uint32_t i = 0; i < length; i++) {
	SCM s_vertex = s_list_ref(s_vertices, int2scm(i));
	Vector vertex = scm2vector(s_vertex, proc, 1);
	mesh->addVertex(vertex);
    }
    cout << "Vertices: " << length << endl;

    // Add the triangles
    assert(scm2bool(s_list_p (s_triangles)));
    length = scm2int(s_length(s_triangles),0,"");
    Vector2 uv = Vector2(0,0);
    uint32_t v[3];
    try {
	for(uint32_t i = 0; i < length; i++) {
	    SCM s_triangle = s_list_ref(s_triangles, int2scm(i));
	    Vector triangle = scm2vector(s_triangle, proc, 2);
	    v[0] = uint32_t(triangle[0]);
	    v[1] = uint32_t(triangle[1]);
	    v[2] = uint32_t(triangle[2]);
	    mesh->addTriangle(v);
	}
    } catch (Exception e) {
	throw scheme_exception"make-mesh", e.getMessage());
    }
    cout << "Faces: " << length << endl;

    return sceneobject2scm(mesh);
}

SCM make_ply_mesh(SCM s_filename, SCM s_material)
{
    char* proc = "make-ply-mesh";
    string filename = scm2string(s_filename);
    Material* material = scm2material(s_material, proc, 2);
    PLY* ply;
    try {
        ply = new PLY(filename, material);
    } catch (Exception e) {
    	throw scheme_exception(proc, e.getMessage());
    }         
    return sceneobject2scm(ply);
}

SCM make_bezierpatch(SCM s_points, SCM s_xres, SCM s_yres, SCM s_material) 
{
    char* proc = "make-bezierpatch";

    vector<Vector> points = scm2vectorlist(s_points, proc,1);
    uint32_t xresolution = scm2int(s_xres, 2, proc);
    uint32_t yresolution = scm2int(s_yres, 3, proc);
    Material* material = scm2material(s_material, proc, 4);

    assert(points.size() == 16);
    BezierPatch* patch = new BezierPatch(points, xresolution, yresolution, material);
    return sceneobject2scm(patch);
}

SCM make_union(SCM s_things) 
{
    char* proc = "make-union";
    Material* material = NULL;
    vector<Solid*> solids;
    
    int i = 1;
    while (s_things != S_EMPTY_LIST) {
        SchemeObject* o = i_car(s_things);
        if (isMaterial(o) && i_cdr(s_things) == S_EMPTY_LIST) {
            material = scm2material(o);        
        } else {
   	    SceneObject* so_solid = scm2sceneobject(s_solid, proc, i);
            Solid* solid = dynamic_cast<Solid*>(so_solid);
            if (solid == NULL) {
                wrong_type_arg(proc, i, s_solid);
            }
            solids.push_back(solid);
        }
        s_things = i_cdr(s_things);
        i++; 
    }

    CSGUnion* csg = new CSGUnion(&solids, material);
    return sceneobject2scm(csg);
}

SCM make_difference(SCM s_left, SCM s_right, SCM s_material) 
{
    char* proc = "make-difference";
    Material* material;
    if (s_material == S_UNSPECIFIED) {
        material = NULL;
    } else {
	material = scm2material(s_material,proc,3);
    }
    SceneObject* so_left = scm2sceneobject(s_left, proc, 1);
    SceneObject* so_right = scm2sceneobject(s_right, proc, 2);

    Solid* solid_left = dynamic_cast<Solid*>(so_left);
    Solid* solid_right = dynamic_cast<Solid*>(so_right);

    if (solid_left == NULL) wrong_type_arg(proc,1,s_left);
    if (solid_right == NULL) wrong_type_arg(proc,2,s_right);

    CSGDifference* difference = new CSGDifference(solid_left,solid_right,material);
    return sceneobject2scm(difference);
}

SCM make_intersection(SCM s_left, SCM s_right, SCM s_material) 
{
    char* proc = "make-intersection";
    Material* material;
    if (s_material == S_UNSPECIFIED) {
        material = NULL;
    } else {
	material = scm2material(s_material,proc,3);
    }
    SceneObject* so_left = scm2sceneobject(s_left, proc, 1);
    SceneObject* so_right = scm2sceneobject(s_right, proc, 2);

    Solid* solid_left = dynamic_cast<Solid*>(so_left);
    Solid* solid_right = dynamic_cast<Solid*>(so_right);

    if (solid_left == NULL) wrong_type_arg(proc,1,s_left);
    if (solid_right == NULL) wrong_type_arg(proc,2,s_right);

    CSGIntersection* intersection = new CSGIntersection(solid_left,solid_right,material);
    return sceneobject2scm(intersection);
}


SCM make_julia(SCM s_c, SCM s_max_iter, SCM s_steps, SCM s_accuracy, SCM s_w_offset, SCM s_material)
{
    char* proc = "make-julia";

    Quaternion c = scm2quaternion(s_c, proc, 1);
    int max_iter = scm2int(s_max_iter, 2, proc);
    int steps = scm2int(s_steps, 3, proc);
    double accuracy = s_scm2double(s_accuracy, 4, proc);
    double w_offset = s_scm2double(s_w_offset, 5, proc);
    Material* material = scm2material(s_material, proc, 6);

    Julia* julia = new Julia(c, max_iter, steps, accuracy, w_offset, material);
    return sceneobject2scm(julia);
}

SCM make_marching_cubes(SCM s_isosurface, SCM s_subdivisions, SCM s_adaptive)
{
    char* proc = "make-marching-cubes";

    SceneObject* sobj = scm2sceneobject(s_isosurface, proc, 1);
    IsoSurface* isosurface = dynamic_cast<IsoSurface*>(sobj);
    if (isosurface == NULL) wrong_type_arg(proc,1,s_isosurface);
    int subdivisions= scm2int(s_subdivisions, 3, proc);
    bool adaptive = scm2bool(s_adaptive);
    MarchingCubes* marching = new MarchingCubes(isosurface, subdivisions, adaptive);
    return sceneobject2scm(marching);
}

SCM make_bound(SCM s_objectgroup)
{
    char* proc = "make-bound";
    SceneObject* sobj = scm2sceneobject(s_objectgroup, proc, 1);
    ObjectGroup* objectgroup = dynamic_cast<ObjectGroup*>(sobj);
    if (objectgroup == NULL) wrong_type_arg(proc,1,s_objectgroup);
    Bound* bound = new Bound(objectgroup);
    return sceneobject2scm(bound);
}

SCM make_instance(SCM s_object, SCM s_material)
{
    char* proc = "make-instance";
    SceneObject* sobj = scm2sceneobject(s_object, proc, 1);
    Object* obj = dynamic_cast<Object*>(sobj);
    if (obj == NULL) wrong_type_arg(proc,1,s_object);

    TransformedInstance* instance;
    if (s_material == S_UNSPECIFIED) {
        instance = new TransformedInstance(obj);
    } else {
	Material* material = scm2material(s_material,proc,3);
        instance = new TransformedInstance(obj, material);
    }
    return sceneobject2scm(instance);
}


SCM bounding_box(SCM s_obj) 
{
    char* proc = "bounding-box";

    SceneObject* sceneobj = scm2sceneobject(s_obj,proc,1);
    Object* obj = dynamic_cast<Object*>(sceneobj);
    if (obj == NULL) wrong_type_arg(proc,1,s_obj);
    AABox bbox = obj->getBoundingBox();
    Vector v1 = bbox.minimum();
    Vector v2 = bbox.maximum();
    SCM s_v1 = vector2scm(v1);
    SCM s_v2 = vector2scm(v2);
    return i_list_2(s_v1, s_v2);
}

void SceneObjectFactory::register_procs(Scheme* scheme) 
{
    scheme->assign("make-sphere",2,1,0,
	    (SCM (*)()) make_sphere);
    scheme->assign("make-ellipsoid",2,1,0,
	    (SCM (*)()) make_ellipsoid);
    scheme->assign("make-box",3,0,0,
	    (SCM (*)()) make_box);
    scheme->assign("make-solid-box",2,1,0,
	    (SCM (*)()) make_solid_box);
    scheme->assign("make-cylinder",3,1,0,
	    (SCM (*)()) make_cylinder);
    scheme->assign("make-uncapped-cylinder",3,1,0,
	    (SCM (*)()) make_uncapped_cylinder);
    scheme->assign("make-cone",4,1,0,
	    (SCM (*)()) make_cone);
    scheme->assign("make-uncapped-cone",4,1,0,
	    (SCM (*)()) make_uncapped_cone);
    scheme->assign("make-torus",2,1,0,
	    (SCM (*)()) make_torus);
    scheme->assign("make-extrusion",6,0,0,
	    (SCM (*)()) make_extrusion);
    scheme->assign("make-heightfield",5,0,0,
	    (SCM (*)()) make_heightfield);
    scheme->assign("make-blob",5,0,0,
	    (SCM (*)()) make_blob);
    scheme->assign("make-isosurface",7,0,0,
	    (SCM (*)()) make_isosurface);
    scheme->assign("make-mesh",3,0,0,
	    (SCM (*)()) make_mesh);
    scheme->assign("make-ply-mesh",2,0,0,
	    (SCM (*)()) make_ply_mesh);
    scheme->assign("make-bezierpatch",4,0,0,
	    (SCM (*)()) make_bezierpatch);
    scheme->assign("make-difference",2,1,0,
	    (SCM (*)()) make_difference);
    scheme->assign("make-intersection",2,1,0,
	    (SCM (*)()) make_intersection);
    scheme->assign("make-union",0,0,1,
	    (SCM (*)()) make_union);
    scheme->assign("make-parametrized-surface",6,0,0,
	    (SCM (*)()) make_parametrized_surface);
    scheme->assign("make-julia",6,0,0,
	    (SCM (*)()) make_julia);
    scheme->assign("make-marching-cubes",3,0,0,
	    (SCM (*)()) make_marching_cubes);
    scheme->assign("make-bound",1,0,0,
	    (SCM (*)()) make_bound);
    scheme->assign("make-instance",1,1,0,
	    (SCM (*)()) make_instance);
    scheme->assign("bounding-box",1,0,0,
	    (SCM (*)()) bounding_box);
}

