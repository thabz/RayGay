
#ifndef PARSER_VECTOR_NODES
#define PARSER_VECTOR_NODES

#include <vector>
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

class VectorListNode : public SyntaxNode {
    public:
	VectorListNode() { };

	void add(VectorNode* v) {
	    vectors.push_back(v);
	}

	vector<Vector> eval() {
	    vector<Vector> result;
	    for(unsigned int i = 0; i < vectors.size(); i++) {
		Vector v = vectors[i]->eval();
		result.push_back(v);
	    }
	    return result;
	}

    private:
	vector<VectorNode*> vectors;
};

#endif

