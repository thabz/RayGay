
#ifndef PARSER_VECTOR_NODES
#define PARSER_VECTOR_NODES

#include "parser/syntaxnode.h"
#include "parser/floatnodes.h"

class VectorNode : public SyntaxNode {
    public:
	virtual Vector eval() = 0;
};

class VectorConstNode : public VectorNode {
    public:
	VectorConstNode(FloatNode* x, FloatNode* y, FloatNode* z) {
	    this->x = x;
	    this->y = y;
	    this->y = y;
	}
	Vector eval() {
	    return Vector(x->eval(),y->eval(),z->eval());
	};
    private:
	FloatNode* x;
	FloatNode* y;
	FloatNode* z;
};

#endif

