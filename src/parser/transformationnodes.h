
#ifndef PARSER_TRANSFORMATION_NODES
#define PARSER_TRANSFORMATION_NODES

#include "parser/syntaxnode.h"
#include "parser/vectornodes.h"
#include "parser/floatnodes.h"
#include "math/matrix.h"

/**
 * Nodes for handling transformations.
 */
class TransformationNode : public SyntaxNode {

    public:
	virtual Matrix eval() {
	    return Matrix();
	}

	virtual ~TransformationNode() { }
};

class TransformationsMultNode : public TransformationNode {
    public:
	TransformationsMultNode(TransformationNode* t1, TransformationNode* t2) {
	    this->t1 = t1;
	    this->t2 = t2;
	}

	virtual ~TransformationsMultNode() {
	    delete t1;
	    delete t2;
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

	virtual ~RotateNode() {
	    delete vec;
	    delete angle;
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

	virtual ~TranslateNode() {
	    delete vec;
	}

	Matrix eval() {
	    Vector v = vec->eval();
	    return Matrix::matrixTranslate(v);
	}

    private:
	VectorNode* vec;
};

class ScaleNode : public TransformationNode {
    public:
	ScaleNode(VectorNode* vec) {
	    this->vec = vec;
	}

	virtual ~ScaleNode() {
	    delete vec;
	}

	Matrix eval() {
	    Vector v = vec->eval();
	    return Matrix::matrixScale(v);
	}

    private:
	VectorNode* vec;
};


#endif
