
#ifndef PARSER_LANG_NODES
#define PARSER_LANG_NODES

#include <iostream>
#include <string>
#include "parser/floatnodes.h"
#include "parser/sceneobjectnodes.h"
#include "parser/assignments.h"
#include "parser/interpreterenv.h"

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

#endif
