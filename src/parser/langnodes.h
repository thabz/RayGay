
#ifndef PARSER_LANG_NODES
#define PARSER_LANG_NODES

#include <iostream>
#include <string>
#include "parser/floatnodes.h"
#include "parser/sceneobjectnodes.h"
#include "parser/lightnodes.h"
#include "parser/cameranode.h"
#include "parser/assignments.h"
#include "parser/interpreterenv.h"
#include "exception.h"

using namespace std;

// Nodes with no return value and only sideeffects
class ActionNode : public SyntaxNode {
    public:
	virtual void eval() = 0;
};

class ActionNodeList : public SyntaxNode {

};

class FloatPrintNode : public ActionNode {
    public:
	FloatPrintNode(FloatNode* node) {
	    this->node = node;
	}
	void eval() { cout << node->eval() << endl; };
    private:
	FloatNode* node;
};

class AssignFloatNode : public ActionNode {
    public:
	AssignFloatNode(string name, FloatNode* node) {
	    this->name = name;
	    this->node = node;
	}

	virtual ~AssignFloatNode() { delete node; }

	void eval() {
	    Assignments::getUniqueInstance()->setNamedFloat(name,node);
	}
    private:
	FloatNode* node;
	string name;
};

class AddSceneObjectToSceneNode : public ActionNode {
    public:
	AddSceneObjectToSceneNode(SceneObjectNode* node) {
	    this->node = node;
	}

	void eval() {
	    InterpreterEnv::getUniqueInstance()->getScene()->addObject(node->eval());
	}

    private:
        SceneObjectNode* node;
};

class AddLightToSceneNode : public ActionNode {
    public:
	AddLightToSceneNode(LightNode* node) {
	    this->node = node;
	}

	void eval() {
	    InterpreterEnv::getUniqueInstance()->getScene()->addLight(node->eval());
	}

    private:
	LightNode* node;
};

class AddCameraToSceneNode : public ActionNode {
    public:
	AddCameraToSceneNode(CameraNode* cam) {
	    this->cam = cam;
	}

	void eval() {
	    InterpreterEnv::getUniqueInstance()->getScene()->setCamera(cam->eval());
	}

    private:
	CameraNode* cam;
};

class ActionListNode : ActionNode {
    public:
	ActionListNode() {};

	virtual ~ActionListNode() {
	    for(unsigned int i = 0; i < actions.size(); i++) {
		delete actions[i];
	    }
	}

	void addAction(ActionNode* action) {
	    actions.push_back(action);
	}

	void eval() {
	    for(unsigned int i = 0; i < actions.size(); i++) {
		actions[i]->eval();
	    }
	}

    private:
	vector<ActionNode*> actions;
};

class RepeatActionNode : public ActionNode {

    public:
	RepeatActionNode(FloatNode* num, ActionListNode* list) {
	    this->num = num;
	    this->list = list;
	}

	virtual ~RepeatActionNode() {
	    delete num;
	    delete list;
	}

	void eval() {
	    int n = int(num->eval());
	    if (n < 0) {
		throw_exception("Can't repeat negative number of times.");
	    }
	    for(int i = 0; i < n; i++) {
		list->eval();
	    }
	}

    private:
	ActionListNode* list;
	FloatNode* num;
};

#endif
