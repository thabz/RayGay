
#ifndef PARSER_VECTOR_NODES
#define PARSER_VECTOR_NODES

#include <vector>
#include "math/vector.h"
#include "parser/syntaxnode.h"
#include "parser/floatnodes.h"
#include "parser/fileposition.h"

/**
 * Nodes for doing operations on Vector
 */
class VectorNode : public ValueNode {
    public:
	VectorNode() { 
	    this->x = NULL;
	    this->y = NULL;
	    this->z = NULL;
	}
	VectorNode(FilePosition pos) : ValueNode(pos) {
	    this->x = NULL;
	    this->y = NULL;
	    this->z = NULL;
	}

	VectorNode(FloatNode* x, FloatNode* y, FloatNode* z) {
	    this->x = x;
	    this->y = y;
	    this->z = z;
	}
	
	virtual ~VectorNode();

	virtual Vector eval() {
	    return Vector(x->eval(),y->eval(),z->eval());
	};

	ValueNode::ValueType getType() { return ValueNode::VECTOR; };

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
	VectorNormalizeNode(VectorNode* v, FilePosition pos) : VectorNode(pos) {
	    this->vecnode = v;
	}

	virtual ~VectorNormalizeNode() {
	    delete vecnode;
	}

	Vector eval() {
	    Vector result = vecnode->eval();
	    if (IS_ZERO(result.length())) {
		runtime_error("Can't normalize a zero vector");
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
	NamedVectorNode(string name, FilePosition pos) : VectorNode(pos) {
	    this->name = name;
	}

	virtual ~NamedVectorNode() {};

	Vector eval() {
	    return Assignments::getUniqueInstance()->getNamedVector(name,getFilePosition());
	}
	
    private:
	string name;
};

#endif

