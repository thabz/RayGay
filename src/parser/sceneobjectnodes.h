
#ifndef PARSER_SCENE_OBJECT_NODES_H
#define PARSER_SCENE_OBJECT_NODES_H

#include "objects/sceneobject.h"
#include "objects/sphere.h"
#include "objects/ellipsoid.h"
#include "objects/torus.h"
#include "objects/object.h"
#include "objects/necklace.h"
#include "objects/solidbox.h"
#include "objects/box.h"
#include "objects/csg.h"
#include "objects/cone.h"
#include "objects/cylinder.h"
#include "objects/extrusion.h"
#include "objects/mesh.h"
#include "objects/wireframe.h"
#include "parser/assignments.h"
#include "parser/transformationnodes.h"
#include "parser/materialnodes.h"
#include "parser/pathnodes.h"
#include "exception.h"

/**
 * Nodes for handling scene-objects.
 */
class SceneObjectNode : public SyntaxNode {

    public:
	virtual SceneObject* eval() = 0;
};


class ObjectListNode {
    public:
	ObjectListNode() {};

	void addSceneObjectNode(SceneObjectNode* node) {
	    nodes.push_back(node);
	}

	virtual ~ObjectListNode() {
	    for(unsigned int i = 0; i < nodes.size(); i++) {
		delete nodes[i];
	    }
	}

	vector<SceneObject*> eval() {
	    vector<SceneObject*> result;
	    for(unsigned int i = 0; i < nodes.size(); i++) {
		result.push_back(nodes[i]->eval());
	    }
	    return result;
	}

    private:
	vector<SceneObjectNode*> nodes;
};

class TransformedSceneObjectNode : public SceneObjectNode {

    public:
	TransformedSceneObjectNode(SceneObjectNode* obj, TransformationNode* t) {
	    this->obj = obj;
	    this->transformation = t;
	}

	virtual ~TransformedSceneObjectNode() {
	    delete obj;
	    delete transformation;
	}

	SceneObject* eval() {
	    SceneObject* o = obj->eval();
	    Matrix m = transformation->eval();
	    o->transform(m);
	    return o;
	}

    private:
	SceneObjectNode* obj;
	TransformationNode* transformation;
};

class SphereNode : public SceneObjectNode {

    public:
	SphereNode(VectorNode* center, FloatNode* r, MaterialNode* mat) {
	    this->center = center;
	    this->radius = r;
	    this->material = mat;
	}

	virtual ~SphereNode() {
	    delete center;
	    delete radius;
	    delete material;
	}

	SceneObject* eval() {
	    Vector c = center->eval();
	    double r = radius->eval();
	    Material* m = material->eval();
	    return new Sphere(c,r,m);
	}

    private:
	VectorNode* center;
	FloatNode* radius;
	MaterialNode* material;
};

class EllipsoidNode : public SceneObjectNode {

    public:
	EllipsoidNode(VectorNode* center, VectorNode* radii, MaterialNode* mat) {
	    this->center = center;
	    this->radii = radii;
	    this->material = mat;
	}

	virtual ~EllipsoidNode() {
	    delete center;
	    delete radii;
	    delete material;
	}

	SceneObject* eval() {
	    Vector c = center->eval();
	    Vector r = radii->eval();
	    Material* m = material->eval();
	    return new Ellipsoid(c,r,m);
	}

    private:
	VectorNode* center;
	VectorNode* radii;
	MaterialNode* material;
};

class TorusNode : public SceneObjectNode {

    public:
	TorusNode(FloatNode* R, FloatNode* r, MaterialNode* mat) {
	    this->R = R;
	    this->r = r;
	    this->material = mat;
	}

	virtual ~TorusNode() {
	    delete R;
	    delete r;
	    delete material;
	}

	SceneObject* eval() {
	    double rd = r->eval();
	    double Rd = R->eval();
	    Material* m = material->eval();
	    return new Torus(rd,Rd,m);
	}

    private:
	FloatNode* R;
	FloatNode* r;
	MaterialNode* material;
};

class CylinderNode : public SceneObjectNode {

    public:
	CylinderNode(VectorNode* begin, VectorNode* end, FloatNode* radius, MaterialNode* mat) {
	    this->begin = begin;
	    this->end = end;
	    this->radius = radius;
	    this->material = mat;
	}

	virtual ~CylinderNode() {
	    delete begin;
	    delete end;
	    delete radius;
	    delete material;
	}

	SceneObject* eval() {
	    Vector v1 = begin->eval();
	    Vector v2 = end->eval();
	    double r = radius->eval();
	    Material* m = material->eval();
	    return new Cylinder(v1,v2,r,true,m);
	}

    private:
	VectorNode* begin;
	VectorNode* end;
	FloatNode* radius;
	MaterialNode* material;
};

class ConeNode : public SceneObjectNode {

    public:
	ConeNode(VectorNode* begin, VectorNode* end, FloatNode* radius_begin, FloatNode* radius_end, MaterialNode* mat) {
	    this->begin = begin;
	    this->end = end;
	    this->radius_begin = radius_begin;
	    this->radius_end = radius_end;
	    this->material = mat;
	}

	virtual ~ConeNode() {
	    delete begin;
	    delete end;
	    delete radius_begin;
	    delete radius_end;
	    delete material;
	}

	SceneObject* eval() {
	    Vector v1 = begin->eval();
	    Vector v2 = end->eval();
	    double r_begin = radius_begin->eval();
	    double r_end = radius_end->eval();
	    Material* m = material->eval();
	    return new Cone(v1,v2,r_begin,r_end,true,m);
	}

    private:
	VectorNode* begin;
	VectorNode* end;
	FloatNode* radius_begin;
	FloatNode* radius_end;
	MaterialNode* material;
};

class NecklaceNode : public SceneObjectNode {

    public:
	NecklaceNode(PathNode* path, FloatNode* num, FloatNode* r, MaterialNode* mat) {
	    this->path = path;
	    this->num = num;
	    this->radius = r;
	    this->material = mat;
	}

	virtual ~NecklaceNode() {
	    delete path;
	    delete num;
	    delete radius;
	    delete material;
	}

	SceneObject* eval() {
	    Path* p = path->eval();
	    int n = int(num->eval());
	    double r = radius->eval();
	    Material* m = material->eval();
	    return new Necklace(p,n,r,m);
	}

    private:
	PathNode* path;
	FloatNode* num;
	FloatNode* radius;
	MaterialNode* material;
    
};

class SolidBoxNode : public SceneObjectNode {

    public:
	SolidBoxNode(VectorNode* c1, VectorNode* c2, MaterialNode* mat) {
	    this->c1 = c1;
	    this->c2 = c2;
	    this->material = mat;
	}

	virtual ~SolidBoxNode() {
	    delete c1;
	    delete c2;
	    delete material;
	}
	
	SceneObject* eval() {
	    Vector v1 = c1->eval();
	    Vector v2 = c2->eval();
	    Material* m = material->eval();
	    return new SolidBox(v1,v2,m);
	}

    private:
	VectorNode* c1;
	VectorNode* c2;
	MaterialNode* material;

};

class BoxNode : public SceneObjectNode {

    public:
	BoxNode(VectorNode* c1, VectorNode* c2, MaterialNode* mat) {
	    this->c1 = c1;
	    this->c2 = c2;
	    this->material = mat;
	}

	virtual ~BoxNode() {
	    delete c1;
	    delete c2;
	    delete material;
	}

	SceneObject* eval() {
	    Vector v1 = c1->eval();
	    Vector v2 = c2->eval();
	    Material* m = material->eval();
	    return new Box(v1,v2,m);
	}

    private:
	VectorNode* c1;
	VectorNode* c2;
	MaterialNode* material;
};

class ExtrusionNode : public SceneObjectNode {

    public:
	ExtrusionNode(PathNode* path, FloatNode* r, FloatNode* segments, FloatNode* pieces, MaterialNode* mat) {
	    this->path = path;
	    this->radius = r;
	    this->segments = segments;
	    this->pieces = pieces;
	    this->material = mat;
	}

	virtual ~ExtrusionNode() {
	    delete path;
	    delete radius;
	    delete segments;
	    delete pieces;
	    delete material;
	}

	SceneObject* eval() {
	    Path* p = path->eval();
	    double r = radius->eval();
	    unsigned int segs = (unsigned int) segments->eval();
	    unsigned int pies = (unsigned int) pieces->eval();
	    Material* m = material->eval();
	    return new Extrusion(*p,r,segs,pies,m);
	}

    private:
	PathNode* path;
	FloatNode* radius;
	FloatNode* segments;
	FloatNode* pieces;
	MaterialNode* material;

};

class DifferenceNode : public SceneObjectNode {

    public:
	DifferenceNode(SceneObjectNode* left, SceneObjectNode* right, MaterialNode* mat) {
	    this->left = left;
	    this->right = right;
	    this->material = mat;
	}

	virtual ~DifferenceNode() {
	    delete left;
	    delete right;
	    delete material;
	}

	SceneObject* eval() {
	    Solid* s1 = dynamic_cast<Solid*>(left->eval());
	    Solid* s2 = dynamic_cast<Solid*>(right->eval());
	    Material* m = material->eval();
	    return new CSGDifference(s1,s2,m);
	}

    private:
	SceneObjectNode* left;
	SceneObjectNode* right;
	MaterialNode* material;
};

class UnionNode : public SceneObjectNode {

    public:
	UnionNode(ObjectListNode* nodes, MaterialNode* mat) {
	    this->nodes = nodes;
	    this->material = mat;
	}

	virtual ~UnionNode() {
	    delete nodes;
	    delete material;
	}

	SceneObject* eval() {
	    vector<Solid*> solids;
	    vector<SceneObject*> sos = nodes->eval();
	    for(unsigned int i = 0; i < sos.size(); i++) {
		Solid* s = dynamic_cast<Solid*>(sos[i]);
		solids.push_back(s);
	    }
	    /*
	    Solid* s1 = dynamic_cast<Solid*>(left->eval());
	    Solid* s2 = dynamic_cast<Solid*>(right->eval());
	    */
	    Material* m = material->eval();
	    return new CSGUnion(&solids,m);
	}

    private:
	ObjectListNode* nodes;
	MaterialNode* material;
};

class IntersectionNode : public SceneObjectNode {

    public:
	IntersectionNode(SceneObjectNode* left, SceneObjectNode* right, MaterialNode* mat) {
	    this->left = left;
	    this->right = right;
	    this->material = mat;
	}

	virtual ~IntersectionNode() {
	    delete left;
	    delete right;
	    delete material;
	}

	SceneObject* eval() {
	    Solid* s1 = dynamic_cast<Solid*>(left->eval());
	    Solid* s2 = dynamic_cast<Solid*>(right->eval());
	    Material* m = material->eval();
	    return new CSGIntersection(s1,s2,m);
	}

    private:
	SceneObjectNode* left;
	SceneObjectNode* right;
	MaterialNode* material;
};

class WireframeNode : public SceneObjectNode {

    public:
	WireframeNode(SceneObjectNode* obj, FloatNode* radius, MaterialNode* mat) {
	    this->obj = obj;
	    this->radius = radius;
	    this->material = mat;
	}

	virtual ~WireframeNode() {
	    delete obj;
	    delete radius;
	    delete material;
	}

	SceneObject* eval() {
	    Mesh* mesh = dynamic_cast<Mesh*>(obj->eval());
	    double r = radius->eval();
	    Material* mat = material->eval();
	    return new Wireframe(mesh,r,mat);
	}

    private:
	SceneObjectNode* obj;
	FloatNode* radius;
	MaterialNode* material;
};

class MeshNode : public SceneObjectNode {
    public:
	MeshNode(VectorListNode* verts, VectorListNode* tris, MaterialNode* m) {
	    this->vertices = verts;
	    this->triangles = tris;
	    this->material = m;
	    this->eval_done = false;
	}

	virtual ~MeshNode() {
	    delete material;
	}

	/**
	 * This can only be called once.
	 */
	SceneObject* eval() {
	    if (eval_done) {
		throw_exception("MeshNode::eval() called twice");
	    }
	    Mesh* mesh = new Mesh(Mesh::MESH_FLAT,material->eval());
	    /// Add vertices
	    vector<Vector> a = vertices->eval();
	    for(unsigned int i = 0; i < a.size(); i++) {
		mesh->addVertex(a[i]);
	    }
	    delete vertices;
	    /// Add triangles 
	    a = triangles->eval();
	    int v[3];
	    for(unsigned int i = 0; i < a.size(); i++) {
		v[0] = int(a[i][0]);
		v[1] = int(a[i][1]);
		v[2] = int(a[i][2]);
		mesh->addTriangle(v);
	    }
	    delete triangles;
	    eval_done = true;
	    return mesh;
	}


    private:
	VectorListNode* vertices;
	VectorListNode* triangles;
	MaterialNode* material;
	bool eval_done;
};

class NamedSceneObjectNode : public SceneObjectNode {
    public:
	NamedSceneObjectNode(string name) {
	    this->name = name;
	}

	virtual ~NamedSceneObjectNode() {}; // TODO: delete from assignments?

	SceneObject* eval() {
	    return Assignments::getUniqueInstance()->getNamedSceneObject(name)->clone();
	}

    private:
	string name;
};

#endif
