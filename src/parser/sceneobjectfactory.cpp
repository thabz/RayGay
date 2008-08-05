
#include "scheme/filenames.h"

#include "environment.h"

#include "parser/sceneobjectfactory.h"
#include "parser/materialfactory.h"
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
#include "objects/text.h"
#include "objects/obj.h"

Scheme* SceneObjectFactory::scheme;

SchemeObject* s_sceneobject_p(Scheme* scheme, SchemeObject* object) 
{
    return isWrappedObjectType(object, SCENEOBJECT);        
}

SchemeObject* make_sphere(Scheme* scheme, SchemeObject* s_center, SchemeObject* s_radius, SchemeObject* s_material) 
{
    wchar_t* proc = L"make-sphere";
    Vector center = scm2vector(s_center, proc, 1);
    double radius = safe_scm2double(s_radius, 2, proc);
    Material* material;
    if (s_material == S_UNSPECIFIED) {
        material = NULL;
    } else {
	material = scm2material(s_material,proc,3);
    }
    Sphere* sphere = new Sphere(center,radius,material);
    return sceneobject2scm(sphere);
}

SchemeObject* make_ellipsoid(Scheme* scheme, SchemeObject* s_center, SchemeObject* s_radii, SchemeObject* s_material) 
{
    wchar_t* proc = L"make-ellipsoid";
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

SchemeObject* make_torus(Scheme* scheme, SchemeObject* s_R, SchemeObject* s_r, SchemeObject* s_material) 
{
    wchar_t* proc = L"make-to   rus";
    double R = safe_scm2double(s_R, 1, proc);
    double r = safe_scm2double(s_r, 2, proc);
    Material* material;
    if (s_material == S_UNSPECIFIED) {
        material = NULL;
    } else {
	material = scm2material(s_material,proc,3);
    }
    Torus* torus = new Torus(R,r,material);
    return sceneobject2scm(torus);
}

SchemeObject* make_box(Scheme* scheme, SchemeObject* s_corner1, SchemeObject* s_corner2, SchemeObject* s_material) 
{
    wchar_t* proc = L"make-box";
    Vector corner1 = scm2vector(s_corner1, proc, 1);
    Vector corner2 = scm2vector(s_corner2, proc, 2);
    Material* material = scm2material(s_material, proc, 3);
    Box* box = new Box(corner1, corner2, material);
    return sceneobject2scm(box);
}

SchemeObject* make_solid_box(Scheme* scheme, SchemeObject* s_corner1, SchemeObject* s_corner2, SchemeObject* s_material) 
{
    wchar_t* proc = L"make-solid-box";
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

SchemeObject* make_cylinder(Scheme* scheme, SchemeObject* s_begin, SchemeObject* s_end, SchemeObject* s_radius, SchemeObject* s_material) 
{
    wchar_t* proc = L"make-cylinder";
    Vector begin = scm2vector(s_begin, proc, 1);
    Vector end = scm2vector(s_end, proc, 2);
    double radius = safe_scm2double(s_radius, 3, proc);
    Material* material;
    if (s_material == S_UNSPECIFIED) {
        material = NULL;
    } else {
	material = scm2material(s_material,proc,4);
    }
    Cylinder* cylinder = new Cylinder(begin, end, radius, true, material);
    return sceneobject2scm(cylinder);
}

SchemeObject* make_uncapped_cylinder(Scheme* scheme, SchemeObject* s_begin, SchemeObject* s_end, SchemeObject* s_radius, SchemeObject* s_material) 
{
    wchar_t* proc = L"make-uncapped-cylinder";
    Vector begin = scm2vector(s_begin, proc, 1);
    Vector end = scm2vector(s_end, proc, 2);
    double radius = safe_scm2double(s_radius, 3, proc);
    Material* material;
    if (s_material == S_UNSPECIFIED) {
        material = NULL;
    } else {
	material = scm2material(s_material,proc,4);
    }
    Cylinder* cylinder = new Cylinder(begin, end, radius, false, material);
    return sceneobject2scm(cylinder);
}

SchemeObject* make_cone(Scheme* scheme, SchemeObject* s_begin, SchemeObject* s_end, SchemeObject* s_radius_begin, SchemeObject* s_radius_end, SchemeObject* s_material) 
{
    wchar_t* proc = L"make-cone";
    Vector begin = scm2vector(s_begin, proc, 1);
    Vector end = scm2vector(s_end, proc, 2);
    double radius_begin = safe_scm2double(s_radius_begin, 3, proc);
    double radius_end = safe_scm2double(s_radius_end, 4, proc);
    Material* material;
    if (s_material == S_UNSPECIFIED) {
        material = NULL;
    } else {
	material = scm2material(s_material,proc,5);
    }
    Cone* cone = new Cone(begin, end, radius_begin, radius_end, true, material);
    return sceneobject2scm(cone);
}

SchemeObject* make_uncapped_cone(Scheme* scheme, SchemeObject* s_begin, SchemeObject* s_end, SchemeObject* s_radius_begin, SchemeObject* s_radius_end, SchemeObject* s_material) 
{
    wchar_t* proc = L"make-uncapped-cone";
    Vector begin = scm2vector(s_begin, proc, 1);
    Vector end = scm2vector(s_end, proc, 2);
    double radius_begin = safe_scm2double(s_radius_begin, 3, proc);
    double radius_end = safe_scm2double(s_radius_end, 4, proc);
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
SchemeObject* make_extrusion(Scheme* scheme, SchemeObject* s_path, SchemeObject* s_circle, SchemeObject* s_twists, SchemeObject* s_segments, SchemeObject* s_pieces, SchemeObject* s_material)
{
    wchar_t* proc = L"make-extrusion";
    Path* path = scm2path(s_path, proc, 1);
    Path* circle = scm2path(s_circle,proc,2);
    double twists = safe_scm2double(s_twists, 3, proc);
    int segments = safe_scm2int(s_segments, 4, proc);
    int pieces = safe_scm2int(s_pieces, 5, proc);
    Material* material = scm2material(s_material, proc, 6);

    Extrusion* extrusion = new Extrusion(*path, *circle, segments, pieces, twists, material);
    return sceneobject2scm(extrusion);
}

SchemeObject* make_heightfield(Scheme* scheme, SchemeObject* s_texture, SchemeObject* s_box, SchemeObject* s_w_div, SchemeObject* s_d_div, SchemeObject* s_material)
{
    wchar_t* proc = L"make-heightfield";

    Texture* texture = scm2texture(s_texture, proc, 1);
    Vector box = scm2vector (s_box, proc, 2);
    int width_divisions = safe_scm2int(s_w_div, 3, proc);
    int depth_divisions = safe_scm2int(s_d_div, 4, proc);
    Material* material = scm2material(s_material, proc, 5);

    HeightField* hf = new HeightField(texture, box[1], box[0], box[2], width_divisions, depth_divisions, material);
    return sceneobject2scm(hf);
}

SchemeObject* make_blob(Scheme* scheme, SchemeObject* s_iso, SchemeObject* s_steps, SchemeObject* s_accuracy, SchemeObject* s_material, SchemeObject* s_atoms)
{
    wchar_t* proc = L"make-blob";
    double iso = safe_scm2double(s_iso, 1, proc);
    int steps = safe_scm2int(s_steps, 2, proc);
    double accuracy = safe_scm2double(s_accuracy, 3, proc);
    Material* material = scm2material(s_material, proc, 4);

    Blob* blob = new Blob(iso, steps, accuracy, material);

    // Add the atoms
    assert(scm2bool(s_list_p (scheme, s_atoms)));
    uint32_t atoms_num = safe_scm2int(s_length(scheme, s_atoms), 0, L"");
    
    for(uint32_t i = 0; i < atoms_num; i++) {
	SchemeObject* s_atom = s_list_ref(scheme, s_atoms, int2scm(i));

	assert(scm2bool(s_list_p (scheme, s_atom)));
	uint32_t length = safe_scm2int(s_length(scheme, s_atom), 0, L"");

	if (length == 3) {
	    SchemeObject* s_center = s_list_ref(scheme, s_atom, int2scm(0));
	    SchemeObject* s_radius = s_list_ref(scheme, s_atom, int2scm(1));
	    SchemeObject* s_weight = s_list_ref(scheme, s_atom, int2scm(2));
	    Vector center = scm2vector(s_center, proc, 1);
	    double radius = safe_scm2double(s_radius, 2, proc);
	    double weight = safe_scm2double(s_weight, 3, proc);
	    blob->addAtom(center, radius, weight);
	} else if (length == 4) {
	    SchemeObject* s_from = s_list_ref(scheme, s_atom, int2scm(0));
	    SchemeObject* s_to = s_list_ref(scheme, s_atom, int2scm(1));
	    SchemeObject* s_radius = s_list_ref(scheme, s_atom, int2scm(2));
	    SchemeObject* s_weight = s_list_ref(scheme, s_atom, int2scm(3));
	    Vector from = scm2vector(s_from, proc, 1);
	    Vector to = scm2vector(s_to, proc, 2);
	    double radius = safe_scm2double(s_radius, 3, proc);
	    double weight = safe_scm2double(s_weight, 4, proc);
	    blob->addAtom(from, to, radius, weight);
	} else {
	    wrong_type_arg(proc,5,s_atoms);
	}
    }

    return sceneobject2scm(blob);
}

SchemeObject* SceneObjectFactory::make_isosurface(Scheme* scheme, SchemeObject* s_proc, SchemeObject* s_vec_lower, SchemeObject* s_vec_higher, SchemeObject* s_iso, SchemeObject* s_steps, SchemeObject* s_accuracy, SchemeObject* s_material)
{
    wchar_t* proc = L"make-isosurface";
    
    assert_arg_procedure_type(proc, 1, s_proc);

    Vector lower = scm2vector (s_vec_lower, proc, 2);
    Vector higher = scm2vector (s_vec_higher, proc, 3);
    double iso = safe_scm2double(s_iso, 4, proc);
    int steps = safe_scm2int(s_steps, 5, proc);
    double accuracy = safe_scm2double(s_accuracy, 6, proc);
    Material* material = scm2material(s_material, proc, 7);

    AABox bbox = AABox(lower,higher);

    SchemeIsosurface* iso_surface  = new SchemeIsosurface(scheme, s_proc, bbox, steps, accuracy, iso, material);
    return sceneobject2scm(iso_surface);
}

SchemeObject* SceneObjectFactory::make_parametrized_surface(Scheme* scheme, SchemeObject* s_proc, SchemeObject* s_u_res, SchemeObject* s_v_res, SchemeObject* s_u_close, SchemeObject* s_v_close, SchemeObject* s_material)
{
    wchar_t* proc = L"make-parametrized-surface";

    uint32_t uRes = safe_scm2int(s_u_res, 2, proc);
    uint32_t vRes = safe_scm2int(s_v_res, 3, proc);
    bool uClose = scm2bool(s_u_close);
    bool vClose = scm2bool(s_v_close);
    Material* material = scm2material(s_material, proc, 6);

    SchemeParametrizedSurface* surface  = new SchemeParametrizedSurface(scheme, s_proc, uRes, vRes, uClose, vClose, material);
    return sceneobject2scm(surface);
}

SchemeObject* make_mesh(Scheme* scheme, SchemeObject* s_material, SchemeObject* s_vertices, SchemeObject* s_triangles)
{
    wchar_t* proc = L"make-mesh";
    uint32_t flength, vlength;

    Material* material = scm2material(s_material, proc, 1);
    Mesh* mesh = new Mesh(Mesh::MESH_PHONG, material);

    // Add the vertices
    assert(scm2bool(s_list_p (scheme, s_vertices)));
    vlength = safe_scm2int(s_length(scheme, s_vertices), 0, L"");
    for(uint32_t i = 0; i < vlength; i++) {
	    SchemeObject* s_vertex = s_list_ref(scheme, s_vertices, int2scm(i));
	    Vector vertex = scm2vector(s_vertex, proc, 1);
	    mesh->addVertex(vertex);
    }

    // Add the triangles
    assert(scm2bool(s_list_p (scheme, s_triangles)));
    flength = safe_scm2int(s_length(scheme, s_triangles), 0, L"");
    Vector2 uv = Vector2(0,0);
    uint32_t v[3];
    try {
	    for(uint32_t i = 0; i < flength; i++) {
	        SchemeObject* s_triangle = s_list_ref(scheme, s_triangles, int2scm(i));
	        Vector triangle = scm2vector(s_triangle, proc, 2);
	        v[0] = uint32_t(triangle[0]);
	        v[1] = uint32_t(triangle[1]);
	        v[2] = uint32_t(triangle[2]);
	        mesh->addTriangle(v);
	    }
    } catch (Exception e) {
        // TODO: Do better ie. convert e.getMessage to wchar_t
        cerr << e.getMessage() << endl;
    	throw scheme_exception(proc);
    }
    
    if (Environment::getUniqueInstance()->isVerbose()) {
        cout << "Created mesh with " << vlength << " vertices and " << flength << " faces." << endl;
    }

    return sceneobject2scm(mesh);
}

SchemeObject* make_ply_mesh(Scheme* scheme, SchemeObject* s_filename, SchemeObject* s_material)
{
    wchar_t* proc = L"make-ply-mesh";
    wstring filename = scm2string(s_filename);
    Material* material = scm2material(s_material, proc, 2);
    PLY* ply;
    try {
        ply = new PLY(SchemeFilenames::toFilename(filename).c_str(), material);
    } catch (Exception e) {
        // TODO: Do better ie. convert e.getMessage to wchar_t
        cerr << e.getMessage() << endl;
    	throw scheme_exception(proc);
    }         
    return sceneobject2scm(ply);
}


SchemeObject* make_obj_mesh(Scheme* scheme, SchemeObject* s_filename, SchemeObject* s_material)
{
    wchar_t* proc = L"make-obj-mesh";
    wstring filename = scm2string(s_filename);
    Material* material = scm2material(s_material, proc, 2);
    OBJ* obj;
    try {
        obj = new OBJ(SchemeFilenames::toFilename(filename), material);
    } catch (Exception e) {
        // TODO: Do better ie. convert e.getMessage to wchar_t
        cerr << e.getMessage() << endl;
    	throw scheme_exception(proc);
    }         
    return sceneobject2scm(obj);
}



SchemeObject* make_bezierpatch(Scheme* scheme, SchemeObject* s_points, SchemeObject* s_xres, SchemeObject* s_yres, SchemeObject* s_material) 
{
    wchar_t* proc = L"make-bezierpatch";

    vector<Vector> points = scm2vectorlist(s_points, proc,1);
    uint32_t xresolution = safe_scm2int(s_xres, 2, proc);
    uint32_t yresolution = safe_scm2int(s_yres, 3, proc);
    Material* material = scm2material(s_material, proc, 4);

    assert(points.size() == 16);
    BezierPatch* patch = new BezierPatch(points, xresolution, yresolution, material);
    return sceneobject2scm(patch);
}

SchemeObject* make_union(Scheme* scheme, int num, SchemeStack::iterator args) 
{
    wchar_t* proc = L"make-union";
    Material* material = NULL;
    vector<Solid*> solids;
    
    for(int i = 0; i < num; i++) {
        SchemeObject* o = args[i];
        if (s_material_p(scheme, o) == S_TRUE && i == num - 1) {
            material = scm2material(o, proc, i);
        } else {
   	        SceneObject* so_solid = scm2sceneobject(o, proc, i+1);
            Solid* solid = dynamic_cast<Solid*>(so_solid);
            if (solid == NULL) {
                wrong_type_arg(proc, i+1, o);
            }
            solids.push_back(solid);
        }
    }

    CSGUnion* csg = new CSGUnion(&solids, material);
    return sceneobject2scm(csg);
}

SchemeObject* make_difference(Scheme* scheme, SchemeObject* s_left, SchemeObject* s_right, SchemeObject* s_material) 
{
    wchar_t* proc = L"make-difference";
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

SchemeObject* make_intersection(Scheme* scheme, SchemeObject* s_left, SchemeObject* s_right, SchemeObject* s_material) 
{
    wchar_t* proc = L"make-intersection";
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


SchemeObject* make_julia(Scheme* scheme, SchemeObject* s_c, SchemeObject* s_max_iter, SchemeObject* s_steps, SchemeObject* s_accuracy, SchemeObject* s_w_offset, SchemeObject* s_material)
{
    wchar_t* proc = L"make-julia";

    Quaternion c = scm2quaternion(s_c, proc, 1);
    int max_iter = safe_scm2int(s_max_iter, 2, proc);
    int steps = safe_scm2int(s_steps, 3, proc);
    double accuracy = safe_scm2double(s_accuracy, 4, proc);
    double w_offset = safe_scm2double(s_w_offset, 5, proc);
    Material* material = scm2material(s_material, proc, 6);

    Julia* julia = new Julia(c, max_iter, steps, accuracy, w_offset, material);
    return sceneobject2scm(julia);
}

SchemeObject* make_marching_cubes(Scheme* scheme, SchemeObject* s_isosurface, SchemeObject* s_subdivisions, SchemeObject* s_adaptive)
{
    wchar_t* proc = L"make-marching-cubes";

    SceneObject* sobj = scm2sceneobject(s_isosurface, proc, 1);
    IsoSurface* isosurface = dynamic_cast<IsoSurface*>(sobj);
    if (isosurface == NULL) wrong_type_arg(proc,1,s_isosurface);
    int subdivisions= safe_scm2int(s_subdivisions, 3, proc);
    bool adaptive = scm2bool(s_adaptive);
    MarchingCubes* marching = new MarchingCubes(isosurface, subdivisions, adaptive);
    return sceneobject2scm(marching);
}

SchemeObject* make_bound(Scheme* scheme, SchemeObject* s_objectgroup)
{
    wchar_t* proc = L"make-bound";
    SceneObject* sobj = scm2sceneobject(s_objectgroup, proc, 1);
    ObjectGroup* objectgroup = dynamic_cast<ObjectGroup*>(sobj);
    if (objectgroup == NULL) wrong_type_arg(proc,1,s_objectgroup);
    Bound* bound = new Bound(objectgroup);
    return sceneobject2scm(bound);
}

SchemeObject* make_instance(Scheme* scheme, SchemeObject* s_object, SchemeObject* s_material)
{
    wchar_t* proc = L"make-instance";
    SceneObject* sobj = scm2sceneobject(s_object, proc, 1);
    Object* obj = dynamic_cast<Object*>(sobj);
    if (obj == NULL) {
        wrong_type_arg(proc,1,s_object);
        if (dynamic_cast<ObjectGroup*>(sobj) != NULL) {
            cout << "Hint: If you're trying to make an transformed instance of an objectgroup, such as a mesh, try wrap it in a (make-bound ...) to make it intersectable." << endl;
            // TODO: Should we just do that bounding transparently to the user - for a simpler experience?
        }
    }

    TransformedInstance* instance;
    if (s_material == S_UNSPECIFIED) {
        instance = new TransformedInstance(obj);
    } else {
	    Material* material = scm2material(s_material,proc,3);
        instance = new TransformedInstance(obj, material);
    }
    return sceneobject2scm(instance);
}


SchemeObject* bounding_box(Scheme* scheme, SchemeObject* s_obj) 
{
    wchar_t* proc = L"bounding-box";

    SceneObject* sceneobj = scm2sceneobject(s_obj,proc,1);
    Object* obj = dynamic_cast<Object*>(sceneobj);
    if (obj == NULL) wrong_type_arg(proc,1,s_obj);
    AABox bbox = obj->getBoundingBox();
    Vector v1 = bbox.minimum();
    Vector v2 = bbox.maximum();
    SchemeObject* s_v1 = vector2scm(v1);
    SchemeObject* s_v2 = vector2scm(v2);
    return i_list_2(s_v1, s_v2);
}

SchemeObject* SceneObjectFactory::make_text(Scheme* scheme, SchemeObject* s_text, SchemeObject* s_ttf_file, SchemeObject* s_size, SchemeObject* s_depth, SchemeObject* s_material) {
    wchar_t* proc = L"make-text";
    wstring str = scm2string(s_text);
    wstring wttf_filename = scm2string(s_ttf_file);
    double size = safe_scm2double(s_size, 3, proc);
    double depth = safe_scm2double(s_depth, 4, proc);
    Material* material = scm2material(s_material,proc,5);

    TrueTypeFont* font = new TrueTypeFont(SchemeFilenames::toFilename(wttf_filename));
    Text* text = new Text(str, font, size, depth, material);
    return sceneobject2scm(text);
}


void SceneObjectFactory::register_procs(Scheme* s) 
{
    scheme = s;
    scheme->assign(L"sceneobject?",1,0,0,
	    (SchemeObject* (*)()) s_sceneobject_p);
    scheme->assign(L"make-sphere",2,1,0,
	    (SchemeObject* (*)()) make_sphere);
    scheme->assign(L"make-ellipsoid",2,1,0,
	    (SchemeObject* (*)()) make_ellipsoid);
    scheme->assign(L"make-box",3,0,0,
	    (SchemeObject* (*)()) make_box);
    scheme->assign(L"make-solid-box",2,1,0,
	    (SchemeObject* (*)()) make_solid_box);
    scheme->assign(L"make-cylinder",3,1,0,
	    (SchemeObject* (*)()) make_cylinder);
    scheme->assign(L"make-uncapped-cylinder",3,1,0,
	    (SchemeObject* (*)()) make_uncapped_cylinder);
    scheme->assign(L"make-cone",4,1,0,
	    (SchemeObject* (*)()) make_cone);
    scheme->assign(L"make-uncapped-cone",4,1,0,
	    (SchemeObject* (*)()) make_uncapped_cone);
    scheme->assign(L"make-torus",2,1,0,
	    (SchemeObject* (*)()) make_torus);
    scheme->assign(L"make-extrusion",6,0,0,
	    (SchemeObject* (*)()) make_extrusion);
    scheme->assign(L"make-heightfield",5,0,0,
	    (SchemeObject* (*)()) make_heightfield);
    scheme->assign(L"make-blob",5,0,0,
	    (SchemeObject* (*)()) make_blob);
    scheme->assign(L"make-isosurface",7,0,0,
	    (SchemeObject* (*)()) SceneObjectFactory::make_isosurface);
    scheme->assign(L"make-mesh",3,0,0,
	    (SchemeObject* (*)()) make_mesh);
    scheme->assign(L"make-ply-mesh",2,0,0,
	    (SchemeObject* (*)()) make_ply_mesh);
    scheme->assign(L"make-obj-mesh",2,0,0,
	    (SchemeObject* (*)()) make_obj_mesh);
    scheme->assign(L"make-bezierpatch",4,0,0,
	    (SchemeObject* (*)()) make_bezierpatch);
    scheme->assign(L"make-difference",2,1,0,
	    (SchemeObject* (*)()) make_difference);
    scheme->assign(L"make-intersection",2,1,0,
	    (SchemeObject* (*)()) make_intersection);
    scheme->assign(L"make-union",2,0,1,
	    (SchemeObject* (*)()) make_union);
    scheme->assign(L"make-parametrized-surface",6,0,0,
	    (SchemeObject* (*)()) SceneObjectFactory::make_parametrized_surface);
    scheme->assign(L"make-julia",6,0,0,
	    (SchemeObject* (*)()) make_julia);
    scheme->assign(L"make-marching-cubes",3,0,0,
	    (SchemeObject* (*)()) make_marching_cubes);
    scheme->assign(L"make-bound",1,0,0,
	    (SchemeObject* (*)()) make_bound);
    scheme->assign(L"make-instance",1,1,0,
	    (SchemeObject* (*)()) make_instance);
    scheme->assign(L"bounding-box",1,0,0,
	    (SchemeObject* (*)()) bounding_box);
    scheme->assign(L"make-text",5,0,0,
	    (SchemeObject* (*)()) SceneObjectFactory::make_text);
}

