#include <cmath>

#include "materials/material.h"
#include "materials/normalperturbers/normalperturber.h"
#include "image/rgb.h"
#include "image/texture.h"
#include "intersection.h"
#include "objects/object.h"
#include "math/matrix.h"
#include "math/vector2.h"

Material::Material() {
    reset();
   _kd = 1.0;
   _ks = 0.0;
   _kt = 0.0;
}

Material::Material(RGB diffuseColor, RGB specularColor) {
    reset();
   _diffuseColor = diffuseColor;
   _specularColor = specularColor;
   _kd = 0.75;
   _ks = 0.30;
   _spec_coeff = 50;
   _kt = 0;
}

Material::Material(RGB diffuseColor, double kd, RGB specularColor, double ks, int spec_coeff) {
    reset();
   _diffuseColor = diffuseColor;
   _specularColor = specularColor;
   _kd = kd;
   _ks = ks;
   _spec_coeff = spec_coeff;
   _kt = 0;
}

void Material::reset() {
   texture_diffuse= NULL;
   texture_bump = NULL;
   normal_perturber = NULL;
   eta = 3;
   gloss_enabled = false;
   no_shadow = false;
   _spec_coeff = 1;
}

Material::~Material() {
   // if (texturemap != NULL) delete texturemap;
   // if (texture_bump != NULL) delete texture_bump;
}

RGB Material::getDiffuseColor(const Intersection& i) const {
    if (texture_diffuse != NULL) {
	    Vector2 uv = i.getUV();
	    return texture_diffuse->getTexel(uv);
    } else {
        return _diffuseColor;
    }
}


Vector Material::bump(const Intersection& i, const Vector& normal) const {
    if (normal_perturber != NULL) {
        return normal_perturber->perturb(i.getPoint(), normal);
    } else if (texture_bump != NULL) {
        double u,v,w,h;
        Vector2 uv = i.getUV();
        u = uv[0]; v = uv[1];

        w = texture_bump->getWidth();
        h = texture_bump->getHeight();

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
    } else {
        return normal;
    }
}

double Material::getBumpValue(double u, double v) const {
    RGB col = texture_bump->getTexel(u,v);
    // Map was been grayscaled when loaded in setBumpmap(), so r == g == b
    double val = col[0];
    if (bumpHeight < 0) val = 1 - val;
    return val;
}

void Material::setDiffuseTexture(Texture* texture) {
    texture_diffuse = texture;
}

/**
 * Specify bumpmap
 *
 * @param texture a greyscale texture
 * @param bumpHeight a value in [0,1]
 */ 
void Material::setBumpTexture(Texture* texture, double bumpHeight) {
    this->texture_bump = texture;
    this->bumpHeight = bumpHeight;
}

/**
 * Enable gloss aka diffuse reflection. 
 *
 * @param gloss_ray Number of rays to sample. More rays means slower rendering but more accurate gloss.
 * @param gloss_angle Max angle of these rays in degrees
 */
void Material::enableGloss(uint32_t gloss_rays, double gloss_angle) {
    this->gloss_enabled = true;
    this->gloss_rays = gloss_rays;
    this->gloss_angle_rad = DEG2RAD(gloss_angle);
}

bool Material::requiresUV() const {
    return texture_diffuse != NULL || texture_bump != NULL;
}

void Material::setNormalPerturber(NormalPerturber* perturber) {
    this->normal_perturber = perturber;

}
