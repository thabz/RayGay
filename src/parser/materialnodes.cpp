
#include "parser/materialnodes.h"

Material* NamedMaterialNode::eval() {
    Material* result;
    result = Assignments::getUniqueInstance()->getNamedMaterial(name,getFilePosition());
    return result;
}

MaterialNode::~MaterialNode() {
    if (diffuse_rgb != NULL) delete diffuse_rgb;
    if (specular_rgb != NULL) delete specular_rgb;
    if (Kd != NULL) delete Kd;
    if (Ks != NULL) delete Ks;
    if (Kt != NULL) delete Kt;
    if (specpow != NULL) delete specpow;
    if (eta != NULL) delete eta;
    if (diffuse_texture != NULL) {
	delete diffuse_texture;
    }
    if (bump_texture != NULL) {
	delete bump_texture;
	delete bump_height;
    }
    if (gloss_num != NULL) {
	delete gloss_num;
	delete gloss_angle;
    }
}

void MaterialNode::reset() {
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
