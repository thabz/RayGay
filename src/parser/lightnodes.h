
#ifndef PARSER_LIGHT_NODES
#define PARSER_LIGHT_NODES

#include "parser/assignments.h"
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
	virtual Lightsource* eval();
};

class ArealightNode : public LightNode {
    public:
	ArealightNode(VectorNode* pos, VectorNode* dir, FloatNode* radius, FloatNode* num, FloatNode* jitter, RGBNode* power) {
	    this->position = pos;
	    this->direction = dir;
	    this->radius = radius;
	    this->num = num;
	    this->jitter = jitter;
	    this->power = power;
	}

	Lightsource* eval() {
	    Vector pos = position->eval();
	    Vector dir = direction->eval();
	    double r = radius->eval();
	    int n = int(num->eval());
	    double j = jitter->eval();
	    Arealight* light = new Arealight(pos,dir,r,n,j);
	    light->setPower(power->eval());
	    return light;
	}

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
	SpotlightNode(VectorNode* pos, VectorNode* look_at, FloatNode* angle, FloatNode* cut_angle, RGBNode* power) {
	    this->position = pos;
	    this->look_at = look_at;
	    this->angle = angle;
	    this->cut_angle = cut_angle;
	    this->power = power;
	}

	Lightsource* eval() {
	    Vector pos = position->eval();
	    Vector look = look_at->eval();
	    double a = angle->eval();
	    double ca = cut_angle->eval();
	    Spotlight* light = new Spotlight(pos,look,a,ca);
	    light->setPower(power->eval());
	    return light;
	}

    private:
	VectorNode* position;
	VectorNode* look_at;
	FloatNode* angle;
	FloatNode* cut_angle;
	RGBNode* power;
};

class PointlightNode : public LightNode {
    public:
	PointlightNode(VectorNode* pos, RGBNode* power) {
	    this->position = pos;
	    this->power = power;
	}

	Lightsource* eval() {
	    Vector pos = position->eval();
	    Pointlight* light = new Pointlight(pos);
	    light->setPower(power->eval());
	    return light;
	}

    private:
	VectorNode* position;
	VectorNode* look_at;
	FloatNode* angle;
	FloatNode* cut_angle;
	RGBNode* power;
};

class SkylightNode : public LightNode {
    public:
	SkylightNode(FloatNode* radius, FloatNode* num, RGBNode* power) {
	    this->radius = radius;
	    this->num = num;
	    this->power = power;
	}

	Lightsource* eval() {
	    double r = radius->eval();
	    int n = int(num->eval());
	    Skylight* light = new Skylight(r,n);
	    light->setPower(power->eval());
	    return light;
	}

    private:
	FloatNode* radius;
	FloatNode* num;
	RGBNode* power;
};

#endif
