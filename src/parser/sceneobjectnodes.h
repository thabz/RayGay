
#ifndef PARSER_SCENE_OBJECT_NODES_H
#define PARSER_SCENE_OBJECT_NODES_H

#include "objects/sceneobject.h"
#include "objects/sphere.h"
#include "objects/torus.h"
#include "objects/object.h"
#include "objects/necklace.h"
#include "objects/solidbox.h"
#include "objects/box.h"
#include "objects/csg.h"
#include "objects/cylinder.h"
#include "objects/extrusion.h"
#include "objects/mesh.h"
#include "objects/wireframe.h"
#include "parser/assignments.h"
#include "parser/transformationnodes.h"
#include "parser/materialnodes.h"
#include "parser/pathnodes.h"

class SceneObjectNode : public SyntaxNode {

    public:
	virtual SceneObject* eval() = 0;
};

class TransformedSceneObjectNode : public SceneObjectNode {

    public:
	TransformedSceneObjectNode(SceneObjectNode* obj, TransformationNode* t) {
	    this->obj = obj;
	    this->transformation = t;
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

class TorusNode : public SceneObjectNode {

    public:
	TorusNode(FloatNode* R, FloatNode* r, MaterialNode* mat) {
	    this->R = R;
	    this->r = r;
	    this->material = mat;
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

class NecklaceNode : public SceneObjectNode {

    public:
	NecklaceNode(PathNode* path, FloatNode* num, FloatNode* r, MaterialNode* mat) {
	    this->path = path;
	    this->num = num;
	    this->radius = r;
	    this->material = mat;
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

	SceneObject* eval() {
	    Solid* s1 = dynamic_cast<Solid*>(left->eval());
	    Solid* s2 = dynamic_cast<Solid*>(right->eval());
	    Material* m = material->eval();
	    return new CSG(s1,CSG::DIFFERENCE,s2,m);
	}

    private:
	SceneObjectNode* left;
	SceneObjectNode* right;
	MaterialNode* material;
};

class UnionNode : public SceneObjectNode {

    public:
	UnionNode(SceneObjectNode* left, SceneObjectNode* right, MaterialNode* mat) {
	    this->left = left;
	    this->right = right;
	    this->material = mat;
	}

	SceneObject* eval() {
	    Solid* s1 = dynamic_cast<Solid*>(left->eval());
	    Solid* s2 = dynamic_cast<Solid*>(right->eval());
	    Material* m = material->eval();
	    return new CSG(s1,CSG::UNION,s2,m);
	}

    private:
	SceneObjectNode* left;
	SceneObjectNode* right;
	MaterialNode* material;
};

class IntersectionNode : public SceneObjectNode {

    public:
	IntersectionNode(SceneObjectNode* left, SceneObjectNode* right, MaterialNode* mat) {
	    this->left = left;
	    this->right = right;
	    this->material = mat;
	}

	SceneObject* eval() {
	    Solid* s1 = dynamic_cast<Solid*>(left->eval());
	    Solid* s2 = dynamic_cast<Solid*>(right->eval());
	    Material* m = material->eval();
	    return new CSG(s1,CSG::INTERSECTION,s2,m);
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

class NamedSceneObjectNode : public SceneObjectNode {
    public:
	NamedSceneObjectNode(string name) {
	    this->name = name;
	}

	virtual ~NamedSceneObjectNode() {}; // TODO: delete from assignments?

	SceneObject* eval() {
	    return Assignments::getUniqueInstance()->getNamedSceneObject(name)->eval();
	}

    private:
	string name;
};

class ObjectGroupNode : public SceneObjectNode {
    public:
	ObjectGroupNode() {};

	virtual ~ObjectGroupNode() {
	    for(unsigned int i = 0; i < nodes.size(); i++) {
		delete nodes[i];
	    }
	}

	void addSceneObjectNode(SceneObjectNode* node) {
	    nodes.push_back(node);
	}

	SceneObject* eval() {
	    ObjectGroup* result = new ObjectGroup();
	    for(unsigned int i = 0; i < nodes.size(); i++) {
		result->addObject(nodes[i]->eval());
	    }
	    return result;
	}

    private:
	vector<SceneObjectNode*> nodes;
};

#endif
