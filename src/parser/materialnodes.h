
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
	    if (diffuse_rgb != NULL) result->setDiffuseColor(diffuse_rgb->eval());
	    if (specular_rgb != NULL) result->setSpecularColor(specular_rgb->eval());
	    if (Kd != NULL) result->setKd(Kd->eval());
	    if (Ks != NULL) result->setKd(Ks->eval());
	    if (Kt != NULL) result->setKd(Kt->eval());
	    if (specpow != NULL) result->setSc(int(specpow->eval()));
	    if (eta != NULL) result->setEta(eta->eval());
	    if (diffuse_texture != NULL) {
		result->setDiffuseTexture(diffuse_texture);
	    }
	    if (bump_texture != NULL) {
		result->setBumpTexture(bump_texture,bump_height->eval());
	    }
	    if (gloss_num != NULL) {
		result->enableGloss(int(gloss_num->eval()),gloss_angle->eval());
	    }
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
	void setBumpTexture(Texture* c, FloatNode* height) {
	    bump_texture = c;
	    bump_height = height;
	}

	void setKd(FloatNode* Kd) { this->Kd = Kd; };
	void setKs(FloatNode* Kt) { this->Ks = Ks; };
	void setKt(FloatNode* Kt) { this->Kt = Kt; };
	void setSpecpow(FloatNode* specpow) { this->specpow = specpow; };
	void setEta(FloatNode* eta) { this->eta = eta; };
	
	void setGloss(FloatNode* gloss_num, FloatNode* gloss_angle) {
	    this->gloss_num = gloss_num; 
	    this->gloss_angle = gloss_angle; 
	};

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
	FloatNode* bump_height;
};

class MaterialNullNode : public MaterialNode {
    public:
	Material* eval() { return NULL; };
};

#endif
