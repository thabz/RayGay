
#pragma implementation "lightnodes.h"

#include "parser/lightnodes.h"

#include "lights/lightsource.h"
#include "lights/arealight.h"
#include "lights/spotlight.h"
#include "lights/pointlight.h"
#include "lights/skylight.h"

ArealightNode::ArealightNode(VectorNode* pos, VectorNode* dir, FloatNode* radius, FloatNode* num, FloatNode* jitter, RGBNode* power) {
    this->position = pos;
    this->direction = dir;
    this->radius = radius;
    this->num = num;
    this->jitter = jitter;
    this->power = power;
}

Lightsource* ArealightNode::eval() {
    Vector pos = position->eval();
    Vector dir = direction->eval();
    double r = radius->eval();
    int n = int(num->eval());
    double j = jitter->eval();
    Arealight* light = new Arealight(pos,dir,r,n,j);
    light->setPower(power->eval());
    return light;
}

ArealightNode::~ArealightNode() {
    delete position;
    delete direction;
    delete radius;
    delete num;
    delete jitter;
    delete power;
}

SpotlightNode::SpotlightNode(VectorNode* pos, VectorNode* look_at, FloatNode* angle, FloatNode* cut_angle, RGBNode* power) {
    this->position = pos;
    this->look_at = look_at;
    this->angle = angle;
    this->cut_angle = cut_angle;
    this->power = power;
}

Lightsource* SpotlightNode::eval() {
    Vector pos = position->eval();
    Vector look = look_at->eval();
    double a = angle->eval();
    double ca = cut_angle->eval();
    Spotlight* light = new Spotlight(pos,look,a,ca);
    light->setPower(power->eval());
    return light;
}

SpotlightNode::~SpotlightNode() {
    delete position;
    delete look_at;
    delete angle;
    delete cut_angle;
    delete power;
}

PointlightNode::PointlightNode(VectorNode* pos, RGBNode* power) {
    this->position = pos;
    this->power = power;
}

Lightsource* PointlightNode::eval() {
    Vector pos = position->eval();
    Pointlight* light = new Pointlight(pos);
    light->setPower(power->eval());
    return light;
}

PointlightNode::~PointlightNode() {
    delete position;
    delete power;
}

SkylightNode::SkylightNode(FloatNode* radius, FloatNode* num, RGBNode* power) {

    this->radius = radius;
    this->num = num;
    this->power = power;
}

Lightsource* SkylightNode::eval() {
    double r = radius->eval();
    int n = int(num->eval());
    Skylight* light = new Skylight(r,n);
    light->setPower(power->eval());
    return light;
}

SkylightNode::~SkylightNode() {
    delete radius;
    delete num;
    delete power;
}
