
#ifndef PARSER_LIGHT_NODES
#define PARSER_LIGHT_NODES

#pragma interface 

#include "parser/syntaxnode.h"
#include "parser/vectornodes.h"
#include "parser/floatnodes.h"
#include "parser/rgbnodes.h"

#include "lights/lightsource.h"
#include "lights/arealight.h"
#include "lights/spotlight.h"
#include "lights/pointlight.h"
#include "lights/skylight.h"

class LightNode : public SyntaxNode {

    public:
	virtual Lightsource* eval() = 0;
};

class ArealightNode : public LightNode {
    public:
	ArealightNode(VectorNode* pos, VectorNode* dir, FloatNode* radius, FloatNode* num, FloatNode* jitter, RGBNode* power);

	Lightsource* eval();

    private:
	VectorNode* position;
	VectorNode* direction;
	FloatNode* radius;
	FloatNode* num;
	FloatNode* jitter;
	RGBNode* power;
};

class SpotlightNode : public LightNode {
    public:
	SpotlightNode(VectorNode* pos, VectorNode* look_at, FloatNode* angle, FloatNode* cut_angle, RGBNode* power);

	Lightsource* eval();

    private:
	VectorNode* position;
	VectorNode* look_at;
	FloatNode* angle;
	FloatNode* cut_angle;
	RGBNode* power;
};

class PointlightNode : public LightNode {
    public:
	PointlightNode(VectorNode* pos, RGBNode* power);
	Lightsource* eval();

    private:
	VectorNode* position;
	VectorNode* look_at;
	FloatNode* angle;
	FloatNode* cut_angle;
	RGBNode* power;
};

class SkylightNode : public LightNode {
    public:
	SkylightNode(FloatNode* radius, FloatNode* num, RGBNode* power);
	Lightsource* eval();

    private:
	FloatNode* radius;
	FloatNode* num;
	RGBNode* power;
};

#endif
