
#ifndef PARSER_RGB_NODES_H
#define PARSER_RGB_NODES_H

#include "parser/syntaxnode.h"
#include "parser/floatnodes.h"
#include "image/rgba.h"
#include "image/rgb.h"

class RGBNode : public SyntaxNode {
    public:
	RGBNode(FloatNode* r, FloatNode* g, FloatNode* b) {
	    this->r = r;
	    this->g = g;
	    this->b = b;
	}
	RGB eval() {
	    return RGB(r->eval(),g->eval(),b->eval());
	};
    private:
	FloatNode* r;
	FloatNode* g;
	FloatNode* b;
};

class RGBANode : public SyntaxNode {
    public:
	RGBANode(FloatNode* r, FloatNode* g, FloatNode* b, FloatNode* a) {
	    this->r = r;
	    this->g = g;
	    this->b = b;
	    this->a = a;
	}

	RGBA eval() {
	    return RGBA(r->eval(),g->eval(),b->eval(),a->eval());
	}

    private:
	FloatNode* r;
	FloatNode* g;
	FloatNode* b;
	FloatNode* a;
};

#endif
