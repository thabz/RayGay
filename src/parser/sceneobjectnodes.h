
#ifndef PARSER_SCENE_OBJECT_NODES_H
#define PARSER_SCENE_OBJECT_NODES_H

#include "objects/sceneobject.h"
#include "objects/sphere.h"
#include "objects/object.h"
#include "parser/transformationnodes.h"
#include "parser/materialnodes.h"

class SceneObjectNode {

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

#endif
