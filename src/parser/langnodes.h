
#ifndef PARSER_LANG_NODES
#define PARSER_LANG_NODES

#include <iostream>
#include <string>
#include "parser/floatnodes.h"
#include "parser/sceneobjectnodes.h"
#include "parser/lightnodes.h"
#include "parser/cameranode.h"
#include "parser/assignments.h"
#include "parser/boolnodes.h"
#include "exception.h"
#include "environment.h"
#include "function.h"

using namespace std;

/**
 * Nodes with no return value and only sideeffects
 */
class ActionNode : public SyntaxNode {
    public:
	virtual void eval() = 0;
	virtual ~ActionNode();
    protected:
	ActionNode();
	ActionNode(FilePosition pos);
};

class NOPAction	: public ActionNode {
    public:
	void eval() {};
	virtual ~NOPAction() {};
};

class FloatPrintNode : public ActionNode {
    public:
	FloatPrintNode(FloatNode* node) {
	    this->node = node;
	}
	
	virtual ~FloatPrintNode() {
	    delete node;
	}
	
	void eval() { cout << node->eval() << endl; };
	
    private:
	FloatNode* node;
};

class VectorPrintNode : public ActionNode {
    public:
	VectorPrintNode(VectorNode* node) {
	    this->node = node;
	}
	
	virtual ~VectorPrintNode() {
	    delete node;
	}
	
	void eval() { cout << node->eval() << endl; };
	
    private:
	VectorNode* node;
};

class SetBackgroundNode : public ActionNode {
    public:
	SetBackgroundNode(RGBA color) {
	    this->color = color;
	    this->texture = NULL;
	}

	SetBackgroundNode(Texture* texture) {
	    this->texture = texture;
	}

	virtual ~SetBackgroundNode() {
	    if (texture != NULL) {
		delete texture;
	    }
	}

	void eval() {
	    Scene* scene = Environment::getUniqueInstance()->getScene();
	    if (texture == NULL) {
		scene->setBackground(color);
	    } else {
		scene->setBackground(texture);
	    }
	}

    private:
	RGBA color;
	Texture* texture;
};

class SetFogNode : public ActionNode {
    public:
	SetFogNode(double dist, RGB color) {
	    this->color = color;
	    this->dist = dist;
	}

	void eval() {
	    Scene* scene = Environment::getUniqueInstance()->getScene();
	    scene->setFog(color,dist);
	}

    private:
	RGB color;
	double dist;
};

class StringPrintNode : public ActionNode {
    public:
	StringPrintNode(string text) {
	    this->text = text;
	}

	virtual ~StringPrintNode() {  }
	
	void eval() { cout << text << endl; };
    private:
	string text;
};

class AssignFloatNode : public ActionNode {
    public:
	AssignFloatNode(string name, FloatNode* node) {
	    this->name = name;
	    this->node = node;
	}

	virtual ~AssignFloatNode() { delete node; }

	void eval() {
	    Assignments::getUniqueInstance()->setNamedFloat(name,node->eval());
	}
    private:
	FloatNode* node;
	string name;
};

class AssignVectorNode : public ActionNode {
    public:
	AssignVectorNode(string name, VectorNode* node) {
	    this->name = name;
	    this->node = node;
	}

	virtual ~AssignVectorNode() { delete node; }

	void eval() {
	    Assignments::getUniqueInstance()->setNamedVector(name,node->eval());
	}
    private:
	VectorNode* node;
	string name;
};

class AssignPathNode : public ActionNode {
    public:
	AssignPathNode(string name, PathNode* node) {
	    this->name = name;
	    this->node = node;
	}

	virtual ~AssignPathNode() { delete node; }

	void eval() {
	    Assignments::getUniqueInstance()->setNamedPath(name,node->eval());
	}
    private:
	PathNode* node;
	string name;
};

class AssignMaterialNode : public ActionNode {
    public:
	AssignMaterialNode(string name, MaterialNode* node) {
	    this->name = name;
	    this->node = node;
	}

	virtual ~AssignMaterialNode() { delete node; }

	void eval() {
	    Assignments::getUniqueInstance()->setNamedMaterial(name,node->eval());
	}
    private:
	MaterialNode* node;
	string name;
};

class AssignSceneObjectNode : public ActionNode {
    public:
	AssignSceneObjectNode(string name, SceneObjectNode* node) {
	    this->name = name;
	    this->node = node;
	}

	virtual ~AssignSceneObjectNode() { 
	    delete node; 
	}

	void eval() {
	    Assignments::getUniqueInstance()->setNamedSceneObject(name,node->eval());
	}
    private:
	SceneObjectNode* node;
	string name;
};


class FloatOpEqualsNode : public ActionNode {
    public:
	FloatOpEqualsNode(string name, char op, FloatNode* node, FilePosition pos) : ActionNode(pos) {
	    this->name = name;
	    this->op = op;
	    this->node = node;
	}

	virtual ~FloatOpEqualsNode() {
	    delete node;
	}

	void eval() {
	    double val = node->eval();
	    double result = Assignments::getUniqueInstance()->getNamedFloat(name,getFilePosition());
	    switch(op) {
		case '+': result += val;
			  break;
		case '-': result -= val;
			  break;
		case '*': result *= val;
			  break;
		case '/': result /= val;
			  break;
	    }
	    Assignments::getUniqueInstance()->setNamedFloat(name,result);
	}

    private:
	FloatNode* node;
	string name;
	char op;
};

class AddSceneObjectToCollectorNode: public ActionNode {
    public:
	AddSceneObjectToCollectorNode(SceneObjectNode* node) {
	    this->node = node;
	}

	virtual ~AddSceneObjectToCollectorNode() {
	    delete node;
	}

	void eval() {
	    SceneObject* obj = node->eval();
	    Environment::getUniqueInstance()->getObjectCollector()->addObject(obj);
	}

    private:
        SceneObjectNode* node;
};

class AddLightToSceneNode : public ActionNode {
    public:
	AddLightToSceneNode(LightNode* node) {
	    this->node = node;
	}

	virtual ~AddLightToSceneNode() {
	    delete node;
	}

	void eval() {
	    Environment::getUniqueInstance()->getScene()->addLight(node->eval());
	}

    private:
	LightNode* node;
};

class AddCameraToSceneNode : public ActionNode {
    public:
	AddCameraToSceneNode(CameraNode* cam) {
	    this->cam = cam;
	}

	virtual ~AddCameraToSceneNode() {
	    delete cam;
	}

	void eval() {
	    Environment::getUniqueInstance()->getScene()->setCamera(cam->eval());
	}

    private:
	CameraNode* cam;
};

/**
 * The heart of any scene file is this list of statements.
 */
class ActionListNode : ActionNode {
    public:
	ActionListNode();
	virtual ~ActionListNode();
	void addAction(ActionNode* action);
	void eval();

    private:
	vector<ActionNode*> actions;
};

class RepeatActionNode : public ActionNode {

    public:
	RepeatActionNode(FloatNode* num, ActionListNode* list);
	virtual ~RepeatActionNode();
	void eval();

    private:
	ActionListNode* list;
	FloatNode* num;
};

class WhileActionNode : public ActionNode {

    public:
	WhileActionNode(BoolNode* cond, ActionListNode* list);
	virtual ~WhileActionNode();
	void eval();

    private:
	ActionListNode* list;
	BoolNode* cond;
};

class DoWhileActionNode : public ActionNode {

    public:
	DoWhileActionNode(ActionListNode* list, BoolNode* cond);
	virtual ~DoWhileActionNode();
	void eval();

    private:
	ActionListNode* list;
	BoolNode* cond;
};

/**
 * Actionnode for if-statements.
 *
 * The eval performs 'if (cond) l1 else l2'. If l2 is NULL
 * only 'if (cond) l1' is performed.
 */
class IfActionNode : public ActionNode {

    public:
	IfActionNode(BoolNode* cond, ActionListNode* l1, ActionListNode* l2);
	virtual ~IfActionNode();
	void eval();

    private:
	ActionListNode* list1;
	ActionListNode* list2;
	BoolNode* cond;
};

/** 
 * This wraps a ModifyNamedFloatNode into an action
 * so that $x++ can be a standalone action and not just part of
 * a Expr
 */
class ModifyNamedFloatActionNode : public ActionNode {
    public:
	ModifyNamedFloatActionNode(FloatNode* node) {
	    this->node = node;
	}
	
	virtual ~ModifyNamedFloatActionNode() {
	    delete node;
	}
	
	void eval() {
	    node->eval();
	}
    private:
	FloatNode* node;
};

class ObjectGroupNode : public SceneObjectNode {
    public:
	ObjectGroupNode(ActionListNode* actions);
	virtual ~ObjectGroupNode();
	SceneObject* eval();

    private:
	ActionListNode* actions;
};

class BlobNode : public SceneObjectNode {

    public:
	BlobNode(FloatNode* iso, FloatNode* weight, FloatNode* steps, FloatNode* accuracy, ObjectGroupNode* spheres, MaterialNode* mat);
	virtual ~BlobNode();
	SceneObject* eval();

    private:
	FloatNode* steps;
	FloatNode* accuracy;
	FloatNode* iso;
	FloatNode* weight;
	ObjectGroupNode* spheres;
	MaterialNode* material;
};

class UnionNode : public SceneObjectNode {

    public:
	UnionNode(ActionListNode* actions, MaterialNode* mat) {
	    this->actions = actions;
	    this->material = mat;
	}

	virtual ~UnionNode() {
	    delete actions;
	    delete material;
	}

	SceneObject* eval();

    private:
	ActionListNode* actions;
	MaterialNode* material;
};


class FuncCallArgs {
    public:
	FuncCallArgs() { };

	void addArg(ValueNode* node) {
	    nodes.push_back(node);
	}

	unsigned int size() {
	    return nodes.size();
	}

	bool getVectorArgValue(int index, Vector* result) {
	    ValueNode* node = nodes[index];
	    VectorNode* v = dynamic_cast<VectorNode*>(node);
	    if (v == NULL) return false;
	    *result = v->eval();
	    return true;
	}

	bool getFloatArgValue(int index, double* result) {
	    ValueNode* node = nodes[index];
	    FloatNode* v = dynamic_cast<FloatNode*>(node);
	    if (v == NULL) return false;
	    *result = v->eval();
	    return true;
	}

    private:
	vector<ValueNode*> nodes;
};


/**
 * This is a node that performs a function call.
 */
class FuncCallNode : public ActionNode {

    public:
	FuncCallNode(LangFunction* function, FuncCallArgs* args) {
	    this->function = function;
	    this->args = args;
	}
	
	virtual ~FuncCallNode() {
	    delete function;
	    delete args;
	}

	void eval() {
	    function->call(args);
	}

    private:
	LangFunction* function;
	FuncCallArgs* args;
};


#endif
