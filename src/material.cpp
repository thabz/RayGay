
#include "material.h"
#include "rgb.h"

Material::Material() {
}

Material::Material(RGB diffuseColor, RGB specularColor) {
   _diffuseColor = diffuseColor;
   _specularColor = specularColor;
   _kd = 0.75;
   _ks = 0.30;
   _spec_coeff = 50;
   transmission_coefficient = 0;
}

Material::Material(RGB diffuseColor, double kd, RGB specularColor, double ks, int spec_coeff) {
   _diffuseColor = diffuseColor;
   _specularColor = specularColor;
   _kd = kd;
   _ks = ks;
   _spec_coeff = spec_coeff;
   transmission_coefficient = 0;
}

Material::~Material() {
}

RGB Material::getDiffuseColor(const Intersection& i) {
    return getDiffuseColor();
}
