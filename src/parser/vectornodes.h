
#ifndef PARSER_VECTOR_NODES
#define PARSER_VECTOR_NODES

#include <vector>
#include "math/vector.h"
#include "parser/syntaxnode.h"
#include "parser/floatnodes.h"

/**
 * Nodes for doing operations on Vector
 */
class VectorNode : public SyntaxNode {
    public:
	VectorNode() { 
	    this->x = NULL;
	    this->y = NULL;
	    this->z = NULL;
	}

	VectorNode(FloatNode* x, FloatNode* y, FloatNode* z) {
	    this->x = x;
	    this->y = y;
	    this->z = z;
	}
	
	virtual ~VectorNode() {
	    if (x != NULL) delete x;
	    if (y != NULL) delete y;
	    if (z != NULL) delete z;
	}

	virtual Vector eval() {
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

	~VectorListNode() {
	    for(unsigned int i = 0; i < vectors.size(); i++) {
		delete vectors[i];
	    }
	    vectors.clear();
	}

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

class VectorLengthNode : public FloatNode {

    public:
	VectorLengthNode(VectorNode* v) {
	    this->vecnode = v;
	}

	virtual ~VectorLengthNode() {
	    delete vecnode;
	}

	double eval() {
	    return vecnode->eval().length();
	}

    private:
	VectorNode* vecnode;
};

class VectorNormalizeNode : public VectorNode {

    public:
	VectorNormalizeNode(VectorNode* v) {
	    this->vecnode = v;
	}

	virtual ~VectorNormalizeNode() {
	    delete vecnode;
	}

	Vector eval() {
	    Vector result = vecnode->eval();
	    if (IS_ZERO(result.length())) {
               // TODO: Throw runtime exception
	       return Vector(0,0,0);
	    }
	    result.normalize();
	    return result;
	}

    private:
	VectorNode* vecnode;
};

class VectorMultNode : public VectorNode {

    public:
	VectorMultNode(VectorNode* v, FloatNode* f) {
	    this->vecnode = v;
	    this->scale = f;
	}

	virtual ~VectorMultNode() {
	    delete vecnode;
	    delete scale;
	}

	Vector eval() {
	    return scale->eval() * vecnode->eval();
	}

    private:
	VectorNode* vecnode;
	FloatNode* scale;
};

class VectorPlusNode : public VectorNode {

    public:
	VectorPlusNode(VectorNode* left, VectorNode* right) {
	    this->left = left;
	    this->right = right;
	}

	virtual ~VectorPlusNode() {
	    delete left;
	    delete right;
	}

	Vector eval() {
	    return left->eval() + right->eval();
	}

    private:
	VectorNode* left;
	VectorNode* right;
};

class VectorMinusNode : public VectorNode {

    public:
	VectorMinusNode(VectorNode* left, VectorNode* right) {
	    this->left = left;
	    this->right = right;
	}

	virtual ~VectorMinusNode() {
	    delete left;
	    delete right;
	}

	Vector eval() {
	    return left->eval() - right->eval();
	}

    private:
	VectorNode* left;
	VectorNode* right;
};



class NamedVectorNode : public VectorNode {
    public:
	NamedVectorNode(string name) {
	    this->name = name;
	}

	virtual ~NamedVectorNode() {}; // TODO: delete from assigments

	Vector eval() {
	    return Assignments::getUniqueInstance()->getNamedVector(name);
	}
	
    private:
	string name;
};

#endif

