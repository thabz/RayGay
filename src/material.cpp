
#include "material.h"
#include "rgb.h"
#include "image.h"
#include "intersection.h"
#include "object.h"

Material::Material() {
}

Material::Material(RGB diffuseColor, RGB specularColor) {
   _diffuseColor = diffuseColor;
   _specularColor = specularColor;
   _kd = 0.75;
   _ks = 0.30;
   _spec_coeff = 50;
   transmission_coefficient = 0;
   texturemap = NULL;
}

Material::Material(RGB diffuseColor, double kd, RGB specularColor, double ks, int spec_coeff) {
   _diffuseColor = diffuseColor;
   _specularColor = specularColor;
   _kd = kd;
   _ks = ks;
   _spec_coeff = spec_coeff;
   transmission_coefficient = 0;
   texturemap = NULL;
}

Material::~Material() {
}

RGB Material::getDiffuseColor(const Intersection& i) const {
    if (texturemap != NULL) {
	double u,v;
	i.getObject()->getUV(i,&u,&v);
        u -= int(u);
	v -= int(v);
	return texturemap->getTexel(u,v);
    } else {
        return getDiffuseColor();
    }
}

void Material::setTexturemap(const std::string& filename) {
    texturemap = new Image(filename);
}

void Material::setBumpmap(const std::string& filename) {
    bumpmap = new Image(filename);
}
