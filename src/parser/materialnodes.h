
#ifndef PARSER_MATERIAL_NODES_H
#define PARSER_MATERIAL_NODES_H

#include "materials/material.h"

class MaterialNode {
    public:
	virtual Material* eval() = 0;
};

class MaterialNullNode {
    public:
	Material* eval() { return NULL; };
};

#endif
