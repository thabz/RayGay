#ifndef PARSER_PATH_NODES_H
#define PARSER_PATH_NODES_H

#include "parser/vectornodes.h"
#include "parser/floatnodes.h"
#include "parser/syntaxnode.h"
#include "parser/assignments.h"
#include "paths/path.h"
#include "paths/circle.h"
#include "paths/spiral.h"
#include "paths/linesegment.h"
#include "paths/catmullromspline.h"

/**
 * Nodes for handling paths.
 */
class PathNode : public SyntaxNode {

    public:
	virtual Path* eval() = 0;
};

class CircleNode : public PathNode {
    public:
	CircleNode(VectorNode* center, FloatNode* r, VectorNode* up) {
	    this->center = center;
	    this->radius = r;
	    this->up = up;
	}

	Path* eval() {
	    Vector c = center->eval();
	    Vector u = up->eval();
	    double r = radius->eval();
	    return new Circle(c,r,u);
	}
	
    private:
	VectorNode* center;
	VectorNode* up;
	FloatNode* radius;
};

class LinesegmentNode : public PathNode {
    public:
	LinesegmentNode(VectorNode* from, VectorNode* to) {
	    this->from = from;
	    this->to = to;
	}

	Path* eval() {
	    Vector f = from->eval();
	    Vector t = to->eval();
	    return new Linesegment(f,t);
	}
	
    private:
	VectorNode* from;
	VectorNode* to;
};

class SpiralNode : public PathNode {
    public:
	SpiralNode(PathNode* path, FloatNode* radius, FloatNode* windings, FloatNode* offset) {
	    this->path = path;
	    this->radius = radius;
	    this->windings = windings;
	    this->offset = offset;
	}

	Path* eval() {
	    Path* p = path->eval();
	    double r = radius->eval();
	    double w = windings->eval();
	    double o = offset->eval();
	    return new Spiral(p,r,w,o);
	}
	
    private:
	PathNode* path;
	FloatNode* radius;
	FloatNode* windings;
	FloatNode* offset;
};

class NamedPathNode : public PathNode {
    public:
	NamedPathNode(string name) {
	    this->name = name;
	}

	virtual ~NamedPathNode() { }

	Path* eval() {
	    return Assignments::getUniqueInstance()->getNamedPath(name);
	}
	
    private:
	string name;
};

class CatmullRomSplineNode : public PathNode {
    public:
	CatmullRomSplineNode(VectorListNode* vectors) {
	    this->vectors = vectors;
	}

	Path* eval() {
	    vector<Vector> ctrl_points = vectors->eval();
	    return new CatmullRomSpline(ctrl_points);
	}

    private:
	VectorListNode* vectors;
};


#endif
