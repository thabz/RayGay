
#ifndef PARSER_FLOAT_NODES
#define PARSER_FLOAT_NODES

#include <cmath>
#include "parser/assignments.h"
#include "parser/syntaxnode.h"

/**
 * These are syntax tree nodes whose eval returns floats (...well, doubles really)
 */
class FloatNode : public SyntaxNode {

    public:
	virtual double eval() = 0;
};

class FloatConstNode : public FloatNode {
    public:
	FloatConstNode(double val) {
	    this->val = val;
	};
	double eval() { return val; };
    private:
	double val;
};

class FloatPlusNode : public FloatNode {
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

class FloatMinusNode : public FloatNode {
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

class FloatDivNode : public FloatNode {
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

class FloatMultNode : public FloatNode {
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

class FloatNegNode : public FloatNode {
    public:
	FloatNegNode(FloatNode* node) {
	    this->node = node;
	}
	double eval() { return -(node->eval()); };
    private:
	FloatNode* node;
};

class FloatSinNode : public FloatNode {
    public:
	FloatSinNode(FloatNode* node) {
	    this->node = node;
	}
	double eval() { return sin(node->eval()); };
    private:
	FloatNode* node;
};

class FloatCosNode : public FloatNode {
    public:
	FloatCosNode(FloatNode* node) {
	    this->node = node;
	}
	double eval() { return cos(node->eval()); };
    private:
	FloatNode* node;
};

class FloatAbsNode : public FloatNode {
    public:
	FloatAbsNode(FloatNode* node) {
	    this->node = node;
	}
	double eval() { return fabs(node->eval()); };
    private:
	FloatNode* node;
};

class FloatRandomNode : public FloatNode {
    public:
	FloatRandomNode(FloatNode* from, FloatNode* to) {
	    this->from = from;
	    this->to = to;
	}

	double eval() {
	    return RANDOM(from->eval(),to->eval());
	}
	
    private:
        FloatNode* from;
        FloatNode* to;
};

class NamedFloatNode : public FloatNode {
    public:
	NamedFloatNode(string name) {
	    this->name = name;
	}

	virtual ~NamedFloatNode() {}; // TODO: delete from assigments

	double eval() {
	    return Assignments::getUniqueInstance()->getNamedFloat(name);
	}
	
    private:
	string name;
};


#endif
