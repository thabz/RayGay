
#include "materials/material.h"
#include "image/rgb.h"
#include "image/image.h"
#include "intersection.h"
#include "object.h"
#include "math/matrix.h"

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
   bumpmap = NULL;
   indice_of_refraction = 3;
}

Material::Material(RGB diffuseColor, double kd, RGB specularColor, double ks, int spec_coeff) {
   _diffuseColor = diffuseColor;
   _specularColor = specularColor;
   _kd = kd;
   _ks = ks;
   _spec_coeff = spec_coeff;
   transmission_coefficient = 0;
   texturemap = NULL;
   bumpmap = NULL;
   indice_of_refraction = 3;
}

Material::~Material() {
   // if (texturemap != NULL) delete texturemap;
   // if (bumpmap != NULL) delete bumpmap;
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

Vector Material::bump(const Intersection& i, const Vector& normal) const {
    if (bumpmap == NULL) {
	return normal;
    } else {
	double u,v,w,h;
	i.getObject()->getUV(i,&u,&v);
        u -= int(u);
	v -= int(v);
	w = bumpmap->getWidth();
	h = bumpmap->getHeight();
	Vector v0 = Vector(0,0,getBumpValue(u,v));
	Vector v1 = Vector(0,-1,getBumpValue(u,v+1.0/h));
	Vector v2 = Vector(1,0,getBumpValue(u+1.0/w,v));
	Vector bumpnormal = Vector::xProduct(v1-v0,v2-v0);
	bumpnormal.normalize();
	Matrix orient = Matrix::matrixOrient(normal,Vector(0,1,0));
	bumpnormal = orient * bumpnormal;
	bumpnormal = (bumpHeight * bumpnormal) + normal;
	bumpnormal.normalize();
	return bumpnormal;
    }
}

double Material::getBumpValue(double u, double v) const {
    RGB col = bumpmap->getBiCubicTexel(u,v);
    return col.length();
}

void Material::setTexturemap(const std::string& filename) {
    texturemap = Image::load(filename);
}

/**
 * Specify bumpmap
 *
 * @param filename A tga file
 * @param bumpHeight a value in [0,1]
 */ 
void Material::setBumpmap(const std::string& filename, double bumpHeight) {
    bumpmap = Image::load(filename);
    this->bumpHeight = bumpHeight;
}
