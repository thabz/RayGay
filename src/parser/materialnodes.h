
#ifndef PARSER_MATERIAL_NODES_H
#define PARSER_MATERIAL_NODES_H

#include "materials/material.h"
#include "parser/syntaxnode.h"
#include "parser/rgbnodes.h"

class MaterialNode : public SyntaxNode {
    public:
	MaterialNode() {
	    diffuse_texture = NULL;
	    bump_texture = NULL;
	    diffuse_rgb = NULL;
	    specular_rgb = NULL;
	    Kd = NULL;
	    Ks = NULL;
	    Kt = NULL;
	    eta = NULL;
	    specpow = NULL;
	    gloss_angle = NULL;
	    gloss_num = NULL;
	}

	virtual Material* eval() {
	    Material* result = new Material();

	    return result;
	    
	};

	void setDiffuseColor(RGBNode* col) {
	    diffuse_rgb = col;
	}

	void setSpecularColor(RGBNode* col) {
	    specular_rgb = col;
	}
	void setDiffuseColor(Texture* t) {
	    diffuse_texture = t;
	}
	void setBumpTexture(Texture* c) {
	    bump_texture = c;
	}

    private:
	RGBNode* diffuse_rgb;
	RGBNode* specular_rgb;
	FloatNode* Kd;
	FloatNode* Ks;
	FloatNode* Kt;
	FloatNode* eta;
	FloatNode* specpow;
	FloatNode* gloss_num;
	FloatNode* gloss_angle;
	Texture* diffuse_texture;
	Texture* bump_texture;
};

class MaterialNullNode : public MaterialNode {
    public:
	Material* eval() { return NULL; };
};

#endif
