
#ifndef PARSER_TRANSFORMATION_NODES
#define PARSER_TRANSFORMATION_NODES

#include "parser/vectornodes.h"
#include "parser/floatnodes.h"
#include "math/matrix.h"

class TransformationNode {

    public:
	virtual Matrix eval() {
	    return Matrix();
	}
};

class TransformationsMultNode : public TransformationNode {
    public:
	TransformationsMultNode(TransformationNode* t1, TransformationNode* t2) {
	    this->t1 = t1;
	    this->t2 = t2;
	}

	Matrix eval() {
	    return t1->eval() * t2->eval();
	}

    private:
	TransformationNode* t1;
	TransformationNode* t2;
};


class RotateNode : public TransformationNode {
    public:
	RotateNode(VectorNode* vec, FloatNode* angle) {
	    this->vec = vec;
	    this->angle = angle;
	}

	Matrix eval() {
	    double a = angle->eval();
	    Vector v = vec->eval();
	    return Matrix::matrixRotate(v,a);
	}

    private:
	VectorNode* vec;
	FloatNode* angle;
};

class TranslateNode : public TransformationNode {
    public:
	TranslateNode(VectorNode* vec) {
	    this->vec = vec;
	}

	Matrix eval() {
	    Vector v = vec->eval();
	    return Matrix::matrixTranslate(v);
	}

    private:
	VectorNode* vec;
	FloatNode* angle;
};



#endif
