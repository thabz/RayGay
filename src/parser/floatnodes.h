
#ifndef PARSER_FLOAT_NODES
#define PARSER_FLOAT_NODES

#include <cmath>
#include "parser/assignments.h"
#include "parser/syntaxnode.h"
#include "parser/fileposition.h"

/**
 * These are syntax tree nodes whose eval returns floats (...well, doubles really)
 * \todo Implement perlin(vector)
 */

class FloatNode : public ValueNode {

    public:
	virtual double eval() = 0;
	virtual ~FloatNode();
	ValueNode::ValueType getType() { return ValueNode::FLOAT; };
    protected:
	FloatNode() {};
	FloatNode(FilePosition pos) : ValueNode(pos) {};
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
	virtual ~FloatPlusNode() {
	    delete left;
	    delete right;
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
	virtual ~FloatMinusNode() {
	    delete left;
	    delete right;
	}
	double eval() { return left->eval() - right->eval(); };
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
	virtual ~FloatDivNode() {
	    delete left;
	    delete right;
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
	virtual ~FloatMultNode() {
	    delete left;
	    delete right;
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
	virtual ~FloatNegNode() {
	    delete node;
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
	virtual ~FloatSinNode() {
	    delete node;
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
	virtual ~FloatCosNode() {
	    delete node;
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
	virtual ~FloatAbsNode() {
	    delete node;
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

	virtual ~FloatRandomNode() {
	    delete from;
	    delete to;
	}

	double eval() {
	    return RANDOM(from->eval(),to->eval());
	}
	
    private:
        FloatNode* from;
        FloatNode* to;
};

class FloatPowNode : public FloatNode {
    public:
	FloatPowNode(FloatNode* v1, FloatNode* v2) {
	    this->v1 = v1;
	    this->v2 = v2;
	}

	virtual ~FloatPowNode() {
	    delete v1;
	    delete v2;
	}

	double eval() {
	    return pow(v1->eval(),v2->eval());
	}
	
    private:
        FloatNode* v1;
        FloatNode* v2;
};

class FloatMaxNode : public FloatNode {
    public:
	FloatMaxNode(FloatNode* v1, FloatNode* v2) {
	    this->v1 = v1;
	    this->v2 = v2;
	}

	virtual ~FloatMaxNode() {
	    delete v1;
	    delete v2;
	}

	double eval() {
	    return max(v1->eval(),v2->eval());
	}
	
    private:
        FloatNode* v1;
        FloatNode* v2;
};

class FloatMinNode : public FloatNode {
    public:
	FloatMinNode(FloatNode* v1, FloatNode* v2) {
	    this->v1 = v1;
	    this->v2 = v2;
	}

	virtual ~FloatMinNode() {
	    delete v1;
	    delete v2;
	}

	double eval() {
	    return min(v1->eval(),v2->eval());
	}
	
    private:
        FloatNode* v1;
        FloatNode* v2;
};

/**
 * This implements the constructs $x++ and $x--  and ++$x and --$x
 */
class ModifyNamedFloatNode : public FloatNode {
    public:
	ModifyNamedFloatNode(string name, char op, bool before,FilePosition pos) : FloatNode(pos) {
	    this->name = name;
	    this->op = op;
	    this->before = before;
	}

	virtual ~ModifyNamedFloatNode() {};

	double eval() {
	    double cur = Assignments::getUniqueInstance()->getNamedFloat(name,getFilePosition());
	    double result = 0;
	    if (before) {
		result = cur;
	    }
	    if (op == '+') {
  		cur += 1;
	    } else {
  		cur -= 1;
	    }
	    if (!before) {
		result = cur;
	    }
	    Assignments::getUniqueInstance()->setNamedFloat(name,cur);
	    return result;
	}

    private:
	string name;
	char op;
	bool before;
};

class NamedFloatNode : public FloatNode {
    public:
	NamedFloatNode(string name, FilePosition pos) : FloatNode(pos) {
	    this->name = name;
	}

	virtual ~NamedFloatNode() {};

	double eval() {
	    return Assignments::getUniqueInstance()->getNamedFloat(name,getFilePosition());
	}
	
    private:
	string name;
};

#endif
