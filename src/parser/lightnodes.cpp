
#include "parser/lightnodes.h"

#include "lights/lightsource.h"
#include "lights/arealight.h"
#include "lights/spotlight.h"
#include "lights/pointlight.h"
#include "lights/skylight.h"

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
