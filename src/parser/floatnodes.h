
#ifndef PARSER_FLOAT_NODES
#define PARSER_FLOAT_NODES

#include <cmath>
#include "parser/assignments.h"
#include "parser/syntaxnode.h"

class FloatNode : public SyntaxNode {

    public:
	virtual double eval() = 0;
};

class FloatConstNode : FloatNode {
    public:
	FloatConstNode(double val) {
	    this->val = val;
	};
	double eval() { return val; };
    private:
	double val;
};

class FloatPlusNode : FloatNode {
    public:
	FloatPlusNode(FloatNode* left, FloatNode* right ) {
	    this->left = left;
	    this->right = right;
	}
	double eval() { return left->eval() + right->eval(); };
    private:
	FloatNode* left;
	FloatNode* right;
};

class FloatMinusNode : FloatNode {
    public:
	FloatMinusNode(FloatNode* left, FloatNode* right ) {
	    this->left = left;
	    this->right = right;
	}
	double eval() { return left->eval() + right->eval(); };
    private:
	FloatNode* left;
	FloatNode* right;
};

class FloatDivNode : FloatNode {
    public:
	FloatDivNode(FloatNode* left, FloatNode* right ) {
	    this->left = left;
	    this->right = right;
	}
	double eval() { return left->eval() / right->eval(); };
    private:
	FloatNode* left;
	FloatNode* right;
};

class FloatMultNode : FloatNode {
    public:
	FloatMultNode(FloatNode* left, FloatNode* right ) {
	    this->left = left;
	    this->right = right;
	}
	double eval() { return left->eval() * right->eval(); };
    private:
	FloatNode* left;
	FloatNode* right;
};

class FloatNegNode : FloatNode {
    public:
	FloatNegNode(FloatNode* node) {
	    this->node = node;
	}
	double eval() { return -(node->eval()); };
    private:
	FloatNode* node;
};

class FloatSinNode : FloatNode {
    public:
	FloatSinNode(FloatNode* node) {
	    this->node = node;
	}
	double eval() { return sin(node->eval()); };
    private:
	FloatNode* node;
};

class FloatCosNode : FloatNode {
    public:
	FloatCosNode(FloatNode* node) {
	    this->node = node;
	}
	double eval() { return cos(node->eval()); };
    private:
	FloatNode* node;
};

class FloatAbsNode : FloatNode {
    public:
	FloatAbsNode(FloatNode* node) {
	    this->node = node;
	}
	double eval() { return fabs(node->eval()); };
    private:
	FloatNode* node;
};

class NamedFloatNode : FloatNode {
    public:
	NamedFloatNode(string name) {
	    this->name = name;
	}

	virtual ~NamedFloatNode() {}; // TODO: delete from assigments

	double eval() {
	    return Assignments::getUniqueInstance()->getNamedFloat(name)->eval();
	}
	
    private:
	string name;
};

#endif
