
#ifndef PARSER_BOOL_NODES_H
#define PARSER_BOOL_NODES_H

#include "parser/syntaxnode.h"

/**
 * Nodes for doing boolean operations.
 */
class BoolNode : public SyntaxNode {

    public:
	virtual bool eval() = 0;
	virtual ~BoolNode() { };
};

class BoolAndNode : public BoolNode {
    public:
	BoolAndNode(BoolNode* left, BoolNode* right) {
	    this->left = left;
	    this->right = right;
	}

	virtual ~BoolAndNode() {
	    delete left;
	    delete right;
	};

	bool eval() {
	    return left->eval() && right->eval();
	}

    private:
	BoolNode* left;
	BoolNode* right;
};

class BoolOrNode : public BoolNode  {
    public:
	BoolOrNode(BoolNode* left, BoolNode* right) {
	    this->left = left;
	    this->right = right;
	}
	
	virtual ~BoolOrNode() { 
	    delete left;
	    delete right;
	};

	bool eval() {
	    return left->eval() || right->eval();
	}

    private:
	BoolNode* left;
	BoolNode* right;
};

class BoolLessThanFNode : public BoolNode  {
    public:
	BoolLessThanFNode(FloatNode* left, FloatNode* right) {
	    this->left = left;
	    this->right = right;
	}

	virtual ~BoolLessThanFNode() { 
	    delete left;
	    delete right;
	};

	bool eval() {
	    return left->eval() < right->eval();
	}

    private:
	FloatNode* left;
	FloatNode* right;
};

class BoolGreaterThanFNode : public BoolNode  {
    public:
	BoolGreaterThanFNode(FloatNode* left, FloatNode* right) {
	    this->left = left;
	    this->right = right;
	}

	virtual ~BoolGreaterThanFNode() { 
	    delete left;
	    delete right;
	};

	bool eval() {
	    return left->eval() > right->eval();
	}

    private:
	FloatNode* left;
	FloatNode* right;
};

class BoolEqualsFNode : public BoolNode  {
    public:
	BoolEqualsFNode(FloatNode* left, FloatNode* right) {
	    this->left = left;
	    this->right = right;
	}

	virtual ~BoolEqualsFNode() { 
	    delete left;
	    delete right;
	};

	bool eval() {
	    return left->eval() == right->eval();
	}

    private:
	FloatNode* left;
	FloatNode* right;
};

class BoolNotNode : public BoolNode  {
    public:
	BoolNotNode(BoolNode* node) {
	    this->node = node;
	}

	virtual ~BoolNotNode() { 
	    delete node;
	};

	bool eval() {
	    return !(node->eval());
	}

    private:
	BoolNode* node;
};


#endif
