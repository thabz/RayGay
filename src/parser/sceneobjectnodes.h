
#ifndef PARSER_SCENE_OBJECT_NODES_H
#define PARSER_SCENE_OBJECT_NODES_H

#include "objects/sceneobject.h"
#include "objects/object.h"
#include "objects/objectgroup.h"
#include "parser/assignments.h"
#include "parser/transformationnodes.h"
#include "parser/materialnodes.h"
#include "parser/pathnodes.h"
#include "parser/fileposition.h"
#include "exception.h"

/**
 * Nodes for handling scene-objects.
 */
class SceneObjectNode : public SyntaxNode {

    public:
	virtual SceneObject* eval() = 0;
	virtual ~SceneObjectNode() {}

    protected:
	SceneObjectNode() {};
	SceneObjectNode(FilePosition pos) : SyntaxNode(pos) {};
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
	TransformedSceneObjectNode(SceneObjectNode* obj, TransformationNode* t);
	virtual ~TransformedSceneObjectNode();
	SceneObject* eval();

    private:
	SceneObjectNode* obj;
	TransformationNode* transformation;
};

class SphereNode : public SceneObjectNode {

    public:
	SphereNode(VectorNode* center, FloatNode* r, MaterialNode* mat);
	virtual ~SphereNode();
	SceneObject* eval();

    private:
	VectorNode* center;
	FloatNode* radius;
	MaterialNode* material;
};

class EllipsoidNode : public SceneObjectNode {

    public:
	EllipsoidNode(VectorNode* center, VectorNode* radii, MaterialNode* mat);
	virtual ~EllipsoidNode();
	SceneObject* eval();

    private:
	VectorNode* center;
	VectorNode* radii;
	MaterialNode* material;
};

class TorusNode : public SceneObjectNode {

    public:
	TorusNode(FloatNode* R, FloatNode* r, MaterialNode* mat); 
	virtual ~TorusNode();
	SceneObject* eval();

    private:
	FloatNode* R;
	FloatNode* r;
	MaterialNode* material;
};

class CylinderNode : public SceneObjectNode {

    public:
	CylinderNode(VectorNode* begin, VectorNode* end, FloatNode* radius, MaterialNode* mat);
	virtual ~CylinderNode();
	SceneObject* eval();

    private:
	VectorNode* begin;
	VectorNode* end;
	FloatNode* radius;
	MaterialNode* material;
};

class ConeNode : public SceneObjectNode {

    public:
	ConeNode(VectorNode* begin, VectorNode* end, FloatNode* radius_begin, FloatNode* radius_end, MaterialNode* mat);
	virtual ~ConeNode();
	SceneObject* eval();

    private:
	VectorNode* begin;
	VectorNode* end;
	FloatNode* radius_begin;
	FloatNode* radius_end;
	MaterialNode* material;
};

class SuperEllipsoidNode : public SceneObjectNode {

    public:
	SuperEllipsoidNode(FloatNode* n1, FloatNode* n2, FloatNode* steps, FloatNode* accuracy, MaterialNode* mat);
	virtual ~SuperEllipsoidNode();
	SceneObject* eval();

    private:
	FloatNode* n1;
	FloatNode* n2;
	FloatNode* steps;
	FloatNode* accuracy;
	MaterialNode* material;
};

class NecklaceNode : public SceneObjectNode {

    public:
	NecklaceNode(PathNode* path, FloatNode* num, FloatNode* r, MaterialNode* mat);
	virtual ~NecklaceNode();
	SceneObject* eval();

    private:
	PathNode* path;
	FloatNode* num;
	FloatNode* radius;
	MaterialNode* material;
    
};

class SolidBoxNode : public SceneObjectNode {

    public:
	SolidBoxNode(VectorNode* c1, VectorNode* c2, MaterialNode* mat);
	virtual ~SolidBoxNode();
	SceneObject* eval();

    private:
	VectorNode* c1;
	VectorNode* c2;
	MaterialNode* material;

};


class BoxNode : public SceneObjectNode {

    public:
	BoxNode(VectorNode* c1, VectorNode* c2, MaterialNode* mat);
	virtual ~BoxNode();
	SceneObject* eval();

    private:
	VectorNode* c1;
	VectorNode* c2;
	MaterialNode* material;
};

class ExtrusionNode : public SceneObjectNode {

    public:
	ExtrusionNode(PathNode* path, FloatNode* r, FloatNode* segments, FloatNode* pieces, MaterialNode* mat);
	virtual ~ExtrusionNode();
	SceneObject* eval();

    private:
	PathNode* path;
	FloatNode* radius;
	FloatNode* segments;
	FloatNode* pieces;
	MaterialNode* material;

};

class DifferenceNode : public SceneObjectNode {

    public:
	DifferenceNode(SceneObjectNode* l, SceneObjectNode* r, MaterialNode* mat);
	virtual ~DifferenceNode();
	SceneObject* eval();

    private:
	SceneObjectNode* left;
	SceneObjectNode* right;
	MaterialNode* material;
};


class IntersectionNode : public SceneObjectNode {

    public:
	IntersectionNode(SceneObjectNode* l, SceneObjectNode* r, MaterialNode* mat);
	virtual ~IntersectionNode();
	SceneObject* eval();

    private:
	SceneObjectNode* left;
	SceneObjectNode* right;
	MaterialNode* material;
};

class WireframeNode : public SceneObjectNode {

    public:
	WireframeNode(SceneObjectNode* obj, FloatNode* radius, MaterialNode* mat);
	virtual ~WireframeNode();
	SceneObject* eval();

    private:
	SceneObjectNode* obj;
	FloatNode* radius;
	MaterialNode* material;
};

class MeshNode : public SceneObjectNode {
    public:
	MeshNode(VectorListNode* verts, VectorListNode* tris, MaterialNode* m);
	virtual ~MeshNode();
	SceneObject* eval();

    private:
	VectorListNode* vertices;
	VectorListNode* triangles;
	MaterialNode* material;
	bool eval_done;
};

class NamedSceneObjectNode : public SceneObjectNode {
    public:
	NamedSceneObjectNode(string name,FilePosition pos) : SceneObjectNode(pos) {
	    this->name = name;
	}

	virtual ~NamedSceneObjectNode() {};

	SceneObject* eval() {
	    return Assignments::getUniqueInstance()->getNamedSceneObject(name,getFilePosition())->clone();
	}

    private:
	string name;
};

class ObjectGroupNode;

class TransformedInstanceNode: public SceneObjectNode {

    public:
	TransformedInstanceNode(SceneObjectNode* obj, MaterialNode* mat, FilePosition pos);
	TransformedInstanceNode(SceneObjectNode* obj, FilePosition pos);
	TransformedInstanceNode(std::string name, MaterialNode* mat, FilePosition pos);
	TransformedInstanceNode(std::string name, FilePosition pos);
	virtual ~TransformedInstanceNode();
	SceneObject* eval();

    private:
	SceneObjectNode* object;
	MaterialNode* material;
	string name;
};

class BoundNode : public SceneObjectNode {

    public:
	BoundNode(SceneObjectNode* obj, FilePosition pos);
	BoundNode(string name, FilePosition pos);
	virtual ~BoundNode();
	SceneObject* eval();

    private:
	SceneObjectNode* object;
	string name;

};


#endif
