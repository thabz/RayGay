#include <cmath>

#include "materials/material.h"
#include "image/rgb.h"
#include "image/image.h"
#include "intersection.h"
#include "objects/object.h"
#include "math/matrix.h"
#include "math/vector2.h"

Material::Material() {
   texturemap = NULL;
   bumpmap = NULL;
   gloss_enabled = false;
   no_shadow = false;
}

Material::Material(RGB diffuseColor, RGB specularColor) {
   _diffuseColor = diffuseColor;
   _specularColor = specularColor;
   _kd = 0.75;
   _ks = 0.30;
   _spec_coeff = 50;
   _kt = 0;
   texturemap = NULL;
   bumpmap = NULL;
   eta = 3;
   repeatY = 1; repeatX = 1;
   gloss_enabled = false;
   no_shadow = false;
}

Material::Material(RGB diffuseColor, double kd, RGB specularColor, double ks, int spec_coeff) {
   _diffuseColor = diffuseColor;
   _specularColor = specularColor;
   _kd = kd;
   _ks = ks;
   _spec_coeff = spec_coeff;
   _kt = 0;
   texturemap = NULL;
   bumpmap = NULL;
   eta = 3;
   repeatY = 1; repeatX = 1;
   gloss_enabled = false;
   no_shadow = false;
}

Material::~Material() {
   // if (texturemap != NULL) delete texturemap;
   // if (bumpmap != NULL) delete bumpmap;
}

RGB Material::getDiffuseColor(const Intersection& i) const {
    if (texturemap != NULL) {
	Vector2 uv = i.getObject()->getUV(i);
	uv = scaleUV(uv);
	return texturemap->getBiCubicTexel(uv[0],uv[1]);
    } else {
        return _diffuseColor;
    }
}

Vector2 Material::scaleUV(const Vector2& uv) const {
    double u = uv[0];
    double v = uv[1];
    u -= int(u);
    v -= int(v);
    u *= repeatX; v *= repeatY;
    return Vector2(u,v);
}

Vector Material::bump(const Intersection& i, const Vector& normal) const {
    if (bumpmap == NULL) {
	return normal;
    } else {
	double u,v,w,h;
	Vector2 uv = i.getObject()->getUV(i);
	uv = scaleUV(uv);
	u = uv[0]; v = uv[1];
	    
	w = bumpmap->getWidth();
	h = bumpmap->getHeight();

	Vector v0 = Vector(0,0,getBumpValue(u,v));
	Vector v1 = Vector(0,-1,getBumpValue(u,v+1.0/h));
	Vector v2 = Vector(1,0,getBumpValue(u+1.0/w,v));
	Vector bumpnormal = Vector::xProduct(v1-v0,v2-v0);
	bumpnormal.normalize();
	Matrix orient = Matrix::matrixOrient(normal,Vector(0,1,0));
	bumpnormal = orient * bumpnormal;
	bumpnormal = (fabs(bumpHeight) * bumpnormal) + normal;
	bumpnormal.normalize();
	return bumpnormal;
    }
}

double Material::getBumpValue(double u, double v) const {
    RGB col = bumpmap->getBiCubicTexel(u,v);
    double val = col[0]; // Map was been grayscaled when loaded in setBumpmap()
    if (bumpHeight < 0) val = 1 - val;
    return val;
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
    bumpmap->grayscale();
    this->bumpHeight = bumpHeight;
}

/**
 * Enable gloss aka diffuse reflection. 
 *
 * @param gloss_ray Number of rays to sample. More rays means slower rendering but more accurate gloss.
 * @param gloss_angle Max angle of these rays in degrees
 */
void Material::enableGloss(unsigned int gloss_rays, double gloss_angle) {
    this->gloss_enabled = true;
    this->gloss_rays = gloss_rays;
    this->gloss_angle_rad = DEG2RAD(gloss_angle);
}

