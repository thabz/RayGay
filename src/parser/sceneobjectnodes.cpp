
#include "parser/sceneobjectnodes.h"
#include <cassert>
#include "objects/mesh.h"
#include "objects/sphere.h"
#include "objects/cylinder.h"
#include "objects/superellipsoid.h"
#include "objects/wireframe.h"
#include "objects/heightfield.h"
#include "objects/csg.h"
#include "objects/extrusion.h"
#include "objects/box.h"
#include "objects/necklace.h"
#include "objects/cone.h"
#include "objects/bound.h"
#include "objects/solidbox.h"
#include "objects/ellipsoid.h"
#include "objects/transformedinstance.h"
#include "objects/torus.h"
#include "types.h"

//---------------------------------------------------------------------
// MeshNode
//---------------------------------------------------------------------
MeshNode::MeshNode(Mesh::MeshType type, VectorListNode* verts, VectorListNode* tris, MaterialNode* m) {
    this->vertices = verts;
    this->triangles = tris;
    this->material = m;
    this->eval_done = false;
    this->type = type;
}

MeshNode::~MeshNode() {
    delete material;
}

/*
 * This can only be called once.
 */
SceneObject* MeshNode::eval() {
    if (eval_done) {
	throw_exception("MeshNode::eval() called twice");
    }
    Mesh* mesh = new Mesh(type, material->eval());
    /// Add vertices
    vector<Vector> a = vertices->eval();
    for(uint i = 0; i < a.size(); i++) {
	mesh->addVertex(a[i]);
    }
    BoundingBox bbox = BoundingBox(a);
    cout << "Mesh bounding box: " << bbox << endl;
    delete vertices;
    /// Add triangles 
    a = triangles->eval();
    uint v[3];
    for(uint i = 0; i < a.size(); i++) {
	v[0] = int(a[i][0]);
	v[1] = int(a[i][1]);
	v[2] = int(a[i][2]);
	mesh->addTriangle(v);
    }
    delete triangles;
    eval_done = true;
    return mesh;
}

//---------------------------------------------------------------------
// SphereNode 
//---------------------------------------------------------------------
SphereNode::SphereNode(VectorNode* center, FloatNode* r, MaterialNode* mat) {
    this->center = center;
    this->radius = r;
    this->material = mat;
}

SphereNode::~SphereNode() {
    delete center;
    delete radius;
    delete material;
}

SceneObject* SphereNode::eval() {
    Vector c = center->eval();
    double r = radius->eval();
    Material* m = material->eval();
    return new Sphere(c,r,m);
}

//---------------------------------------------------------------------
// CylinderNode 
//---------------------------------------------------------------------
CylinderNode::CylinderNode(VectorNode* begin, VectorNode* end, FloatNode* radius, MaterialNode* mat) {
    this->begin = begin;
    this->end = end;
    this->radius = radius;
    this->material = mat;
}

CylinderNode::~CylinderNode() {
    delete begin;
    delete end;
    delete radius;
    delete material;
}

SceneObject* CylinderNode::eval() {
    Vector v1 = begin->eval();
    Vector v2 = end->eval();
    double r = radius->eval();
    Material* m = material->eval();
    return new Cylinder(v1,v2,r,true,m);
}

//---------------------------------------------------------------------
// SuperEllipsoidNode
//---------------------------------------------------------------------
SuperEllipsoidNode::SuperEllipsoidNode(FloatNode* n1, FloatNode* n2, FloatNode* steps, FloatNode* accuracy, MaterialNode* mat) {
    this->n1 = n1;
    this->n2 = n2;
    this->steps = steps;
    this->accuracy = accuracy;
    this->material = mat;
}

SuperEllipsoidNode::~SuperEllipsoidNode() {
    delete n1;
    delete n2;
    delete steps;
    delete accuracy;
    delete material;
}

SceneObject* SuperEllipsoidNode::eval() {
    double _n1 = n1->eval();
    double _n2 = n2->eval();
    uint _steps = (uint) steps->eval();
    double _accuracy = accuracy->eval();
    Material* m = material->eval();
    return new SuperEllipsoid(_n1,_n2,_steps,_accuracy,m);
}

//---------------------------------------------------------------------
// WireframeNode
//---------------------------------------------------------------------
WireframeNode::WireframeNode(SceneObjectNode* obj, FloatNode* radius, MaterialNode* mat) {
    this->obj = obj;
    this->radius = radius;
    this->material = mat;
}

WireframeNode::~WireframeNode() {
    delete obj;
    delete radius;
    delete material;
}

SceneObject* WireframeNode::eval() {
    Mesh* mesh = dynamic_cast<Mesh*>(obj->eval());
    double r = radius->eval();
    Material* mat = material->eval();
    return new Wireframe(mesh,r,mat);
}

//---------------------------------------------------------------------
// TransformedSceneObjectNode
//---------------------------------------------------------------------
TransformedSceneObjectNode::TransformedSceneObjectNode(SceneObjectNode* obj, TransformationNode* t) {
    this->obj = obj;
    this->transformation = t;
}

TransformedSceneObjectNode::~TransformedSceneObjectNode() {
    delete obj;
    delete transformation;
}

SceneObject* TransformedSceneObjectNode::eval() {
    SceneObject* o = obj->eval();
    Matrix m = transformation->eval();
    o->transform(m);
    return o;
}

//---------------------------------------------------------------------
// IntersectionNode 
//---------------------------------------------------------------------
IntersectionNode::IntersectionNode(SceneObjectNode* left, SceneObjectNode* right, MaterialNode* mat) {
    this->left = left;
    this->right = right;
    this->material = mat;
}

IntersectionNode::~IntersectionNode() {
    delete left;
    delete right;
    delete material;
}

SceneObject* IntersectionNode::eval() {
    Solid* s1 = dynamic_cast<Solid*>(left->eval());
    Solid* s2 = dynamic_cast<Solid*>(right->eval());
    Material* m = material->eval();
    return new CSGIntersection(s1,s2,m);
}

//---------------------------------------------------------------------
// DifferenceNode
//---------------------------------------------------------------------
DifferenceNode::DifferenceNode(SceneObjectNode* left, SceneObjectNode* right, MaterialNode* mat) {
    this->left = left;
    this->right = right;
    this->material = mat;
}

DifferenceNode::~DifferenceNode() {
    delete left;
    delete right;
    delete material;
}

SceneObject* DifferenceNode::eval() {
    Solid* s1 = dynamic_cast<Solid*>(left->eval());
    Solid* s2 = dynamic_cast<Solid*>(right->eval());
    Material* m = material->eval();
    return new CSGDifference(s1,s2,m);
}

//---------------------------------------------------------------------
// ExtrusionNode
//---------------------------------------------------------------------
ExtrusionNode::ExtrusionNode(PathNode* path, FloatNode* r, FloatNode* segments, FloatNode* pieces, MaterialNode* mat) {
    this->path = path;
    this->radius = r;
    this->segments = segments;
    this->pieces = pieces;
    this->material = mat;
}

ExtrusionNode::~ExtrusionNode() {
    delete path;
    delete radius;
    delete segments;
    delete pieces;
    delete material;
}

SceneObject* ExtrusionNode::eval() {
    Path* p = path->eval();
    double r = radius->eval();
    uint segs = (uint) segments->eval();
    uint pies = (uint) pieces->eval();
    Material* m = material->eval();
    return new Extrusion(*p,r,segs,pies,m);
}

//---------------------------------------------------------------------
// BoxNode
//---------------------------------------------------------------------
BoxNode::BoxNode(VectorNode* c1, VectorNode* c2, MaterialNode* mat) {
    this->c1 = c1;
    this->c2 = c2;
    this->material = mat;
}

BoxNode::~BoxNode() {
    delete c1;
    delete c2;
    delete material;
}

SceneObject* BoxNode::eval() {
    Vector v1 = c1->eval();
    Vector v2 = c2->eval();
    Material* m = material->eval();
    assert(m != NULL);
    return new Box(v1,v2,m);
}

//---------------------------------------------------------------------
// NecklaceNode
//---------------------------------------------------------------------
NecklaceNode::NecklaceNode(PathNode* path, FloatNode* num, FloatNode* r, MaterialNode* mat) {
    this->path = path;
    this->num = num;
    this->radius = r;
    this->material = mat;
}

NecklaceNode::~NecklaceNode() {
    delete path;
    delete num;
    delete radius;
    delete material;
}

SceneObject* NecklaceNode::eval() {
    Path* p = path->eval();
    int n = int(num->eval());
    double r = radius->eval();
    Material* m = material->eval();
    return new Necklace(p,n,r,m);
}

//---------------------------------------------------------------------
// ConeNode
//---------------------------------------------------------------------
ConeNode::ConeNode(VectorNode* begin, VectorNode* end, FloatNode* radius_begin, FloatNode* radius_end, MaterialNode* mat) {
    this->begin = begin;
    this->end = end;
    this->radius_begin = radius_begin;
    this->radius_end = radius_end;
    this->material = mat;
}

ConeNode::~ConeNode() {
    delete begin;
    delete end;
    delete radius_begin;
    delete radius_end;
    delete material;
}

SceneObject* ConeNode::eval() {
    Vector v1 = begin->eval();
    Vector v2 = end->eval();
    double r_begin = radius_begin->eval();
    double r_end = radius_end->eval();
    Material* m = material->eval();
    return new Cone(v1,v2,r_begin,r_end,true,m);
}

//---------------------------------------------------------------------
// SolidBoxNode
//---------------------------------------------------------------------
SolidBoxNode::SolidBoxNode(VectorNode* c1, VectorNode* c2, MaterialNode* mat) {
    this->c1 = c1;
    this->c2 = c2;
    this->material = mat;
}

SolidBoxNode::~SolidBoxNode() {
    delete c1;
    delete c2;
    delete material;
}

SceneObject* SolidBoxNode::eval() {
    Vector v1 = c1->eval();
    Vector v2 = c2->eval();
    Material* m = material->eval();
    return new SolidBox(v1,v2,m);
}

//---------------------------------------------------------------------
// EllipsoidNode
//---------------------------------------------------------------------
EllipsoidNode::EllipsoidNode(VectorNode* center, VectorNode* radii, MaterialNode* mat) {
	    this->center = center;
	    this->radii = radii;
	    this->material = mat;
	}

EllipsoidNode::~EllipsoidNode() {
	    delete center;
	    delete radii;
	    delete material;
	}

	SceneObject* EllipsoidNode::eval() {
	    Vector c = center->eval();
	    Vector r = radii->eval();
	    Material* m = material->eval();
	    return new Ellipsoid(c,r,m);
	}

//---------------------------------------------------------------------
// TorusNode
//---------------------------------------------------------------------
TorusNode::TorusNode(FloatNode* R, FloatNode* r, MaterialNode* mat) {
    this->R = R;
    this->r = r;
    this->material = mat;
}

TorusNode::~TorusNode() {
    delete R;
    delete r;
    delete material;
}

SceneObject* TorusNode::eval() {
    double rd = r->eval();
    double Rd = R->eval();
    Material* m = material->eval();
    return new Torus(rd,Rd,m);
}

//---------------------------------------------------------------------
// Transformed Instance
//---------------------------------------------------------------------
//
TransformedInstanceNode::TransformedInstanceNode(SceneObjectNode* o, FilePosition pos) : SceneObjectNode(pos) {
    this->object = o;
    this->material = NULL;
}

TransformedInstanceNode::TransformedInstanceNode(SceneObjectNode* o, MaterialNode* m, FilePosition pos) : SceneObjectNode(pos) {
    this->object = o;
    this->material = m;
}

TransformedInstanceNode::TransformedInstanceNode(std::string name, MaterialNode* m, FilePosition pos) : SceneObjectNode(pos) {
    this->object = NULL;
    this->name = name;
    this->material = m;
}

TransformedInstanceNode::TransformedInstanceNode(std::string name, FilePosition pos) : SceneObjectNode(pos) {
    this->object = NULL;
    this->name = name;
    this->material = NULL;
}

TransformedInstanceNode::~TransformedInstanceNode() {
    delete object;
    if (material != NULL) {
	delete material;
    }
}

SceneObject* TransformedInstanceNode::eval() {
    Object* o = NULL;

    if (object != NULL) {
	SceneObject* so = object->eval();
	o = dynamic_cast<Object*>(so);
    } else { 
	SceneObject* so = Assignments::getUniqueInstance()->getNamedSceneObject(name,getFilePosition());
	o = dynamic_cast<Object*>(so);
    }
    if (o == NULL) {
	runtime_error("Transformed instance must be a direct renderable, ie. subclass of Object");
    }

    if (material != NULL) {
	Material* m = material->eval();
	return new TransformedInstance(o,m);
    } else {
	return new TransformedInstance(o);
    }
}

//---------------------------------------------------------------------
// Bounded object
//---------------------------------------------------------------------

BoundNode::BoundNode(SceneObjectNode* so, FilePosition pos) : SceneObjectNode(pos) {
    this->object = so;
}

BoundNode::BoundNode(string name, FilePosition pos) : SceneObjectNode(pos) {
    this->object = NULL;
    this->name = name;
    
}

BoundNode::~BoundNode() {
    delete object;
}

SceneObject* BoundNode::eval() {
    ObjectGroup* o;
    if (object != NULL) {
	SceneObject* so = object->eval();
	o = dynamic_cast<ObjectGroup*>(so);
    } else {
	SceneObject* so = Assignments::getUniqueInstance()->getNamedSceneObject(name,getFilePosition());
	o = dynamic_cast<ObjectGroup*>(so);

    }
    if (o == NULL) {
	runtime_error("Bounded object must be an group, ie. mesh, wireframe, necklace.");
    }
    return new Bound(o);
}

//---------------------------------------------------------------------
// Heightfield
//---------------------------------------------------------------------

HeightFieldNode::HeightFieldNode(Texture* texture, VectorNode* size, FloatNode* width_divisions, FloatNode* depth_divisions, MaterialNode* material) {
    this->texture = texture;
    this->size = size;
    this->width_divisions = width_divisions;
    this->depth_divisions = depth_divisions;
    this->material = material;
}

HeightFieldNode::~HeightFieldNode() {
    delete size;
    delete width_divisions;
    delete depth_divisions;
    delete material;
}

SceneObject* HeightFieldNode::eval() {
    Vector s = size->eval();
    int w_div = (int) width_divisions->eval();
    int d_div = (int) depth_divisions->eval();
    Material* mat = material->eval();
    return new HeightField(texture, s[1], s[0], s[2], w_div, d_div, mat);
}

