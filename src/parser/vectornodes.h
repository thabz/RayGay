
#ifndef PARSER_VECTOR_NODES
#define PARSER_VECTOR_NODES

#include "math/vector.h"
#include "parser/syntaxnode.h"
#include "parser/floatnodes.h"

class VectorNode : public SyntaxNode {
    public:
	VectorNode(FloatNode* x, FloatNode* y, FloatNode* z) {
	    this->x = x;
	    this->y = y;
	    this->z = z;
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

