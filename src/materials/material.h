
#ifndef MATERIAL_H
#define MATERIAL_H

#include "image/rgb.h"
#include "math/vector.h"

class Intersection;
class Image;
class Vector2;

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
	virtual RGB getDiffuseColor(const Intersection& i) const;
	void setDiffuseColor(RGB diffuseColor) { _diffuseColor = diffuseColor; }; ///< Set the diffuse color

	RGB getSpecularColor() const { return _specularColor; }; ///< Get the specular color
	void setSpecularColor(RGB specularColor) { _specularColor = specularColor; };	///< Set the specular color
	Vector bump(const Intersection& i, const Vector& normal) const;

	void setTexturemap(const std::string& filename);
	void setBumpmap(const std::string& filename, double bumpHeight);
	void setRepeatX(const unsigned int repeatX) { this->repeatX = repeatX; };
	void setRepeatY(const unsigned int repeatY) { this->repeatY = repeatY; };
	

	double getKd() const { return _kd; } ;
	void setKd(double kd) { _kd = kd; };

	double getKs() const { return _ks; } ;
	void setKs(double ks) { _ks = ks; };

	int getSc() const { return _spec_coeff; } ;
	void setSc(int sc) { _spec_coeff = sc; };


	/// Set the transmission coefficent (0 = solid, 1 = full transparent)
	void setKt(double kt) { this->_kt = kt; };
	/// Get the transmission coefficent (0 = solid, 1 = full transparent)
	double getKt() const { return _kt; };

	/// Set indice of refraction aka \f$ \eta \f$. Vacuum = 1.0, glas ~1.2 and other materials up to 2-3.
	void setEta(double eta) { this->eta = eta; };
	/// Get indice of refraction aka \f$ \eta \f$
	double getEta() const { return eta; };


	/// Enable gloss
	void enableGloss(unsigned int gloss_rays, double gloss_angle);
	/// Says whether gloss aka diffuse reflection is enabled
	bool glossEnabled() const { return gloss_enabled; };
	/// Number of reflection rays to sample
	unsigned int glossRaysNum() const { return gloss_rays; };
	/// Return gloss max angle between rays in radians
	double glossMaxAngle() const { return gloss_angle_rad; };

	void setNoShadow(bool b) { no_shadow = b; };
	bool noShadow() const { return no_shadow; };

    protected:
	double getBumpValue(double u, double v) const;
	Vector2 scaleUV(const Vector2& v) const;

	    /* Fields */
	RGB _diffuseColor;
	double _kd;

	RGB _specularColor;
        double _ks;

	int _spec_coeff;

	bool no_shadow;

	bool gloss_enabled;
	unsigned int gloss_rays;
	double gloss_angle_rad;

	double bumpHeight;
	unsigned int repeatX; unsigned int repeatY;
	Image* texturemap;
	Image* bumpmap;

	double _kt;
	double eta;
};

#endif
