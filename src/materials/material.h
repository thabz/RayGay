
#ifndef MATERIAL_H
#define MATERIAL_H

#include "image/rgb.h"

class Intersection;
class Image;

enum material_types {
    MATERIAL_SOLID,
    MATERIAL_TEXTURED,
    MATERIAL_PROCEDURAL
}; 

/// Class defining a material
class Material {
    public:
	Material(); ///< Default constructor
	Material(RGB diffuseColor, RGB specularColor);
        Material(RGB diffuseColor, double kd, RGB specularColor, double ks, int spec_coeff);
	virtual ~Material(); ///< Default destructor

	/*!  Get the diffuse color */
	RGB getDiffuseColor() const { return _diffuseColor; };
	void setDiffuseColor(RGB diffuseColor) { _diffuseColor = diffuseColor; }; ///< Set the diffuse color

	RGB getSpecularColor() const { return _specularColor; }; ///< Get the specular color
	void setSpecularColor(RGB specularColor) { _specularColor = specularColor; };	///< Set the specular color
	Vector bump(const Intersection& i, const Vector& normal) const;

	void setTexturemap(const std::string& filename);
	void setBumpmap(const std::string& filename, double bumpHeight);
	
	virtual RGB getDiffuseColor(const Intersection& i) const;

	double getKd() const { return _kd; } ;
	void setKd(double kd) { _kd = kd; };

	double getKs() const { return _ks; } ;
	void setKs(double ks) { _ks = ks; };

	int getSc() const { return _spec_coeff; } ;
	void setSc(int sc) { _spec_coeff = sc; };

	/* See page 757 */
	double transmission_coefficient; ///< The alpha channel (0 = solid, 1 = full transparent)
	double indice_of_refraction; ///< vacuum = 1.0. Glas ~1.2. Other materials up to 2-3.

    private:
	double getBumpValue(double u, double v) const;

	    /* Fields */
	RGB _diffuseColor;
	double _kd;

	RGB _specularColor;
        double _ks;

	int _spec_coeff;

	double bumpHeight;
	int bumpRepeatU;
	int bumpRepeatV;
	Image* texturemap;
	Image* bumpmap;
};

#endif
