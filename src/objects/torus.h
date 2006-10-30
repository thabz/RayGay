
#ifndef TORUS_H
#define TORUS_H

#include "objects/solid.h"
#include "transformer.h"

/**
 * A mathematical torus.
 *
 * A torus are the points satisfying 
 *
 * \f[ (x^2 + y^2 + z^2 + R^2 - r^2)^2 - 4R^2(x^2+y^2) = 0 \f]
 * 
 * with volume
 *
 * \f[ V = \frac{1}{4}\pi^2(r+R)(R-r)^2 \f]
 * 
 * and surface area
 *
 * \f[ A = \pi^2(R^2 - r^2) \f]
 *
 * We solve the torus-ray intersection by letting torus be defined by
 * 
 * \f[ T(x,y,z) =  (x^2 + y^2 + z^2 + R^2 - r^2)^2 - 4R^2(x^2+y^2) \f]
 *
 * and ray be defined by
 * 
 * \f[ D(t) = (p_x,p_y,p_z) + t(d_x,d_y,d_z) \f]
 * 
 * Expanding \f$ T(D(t)) = 0 \f$ gives the quartic equation
 * 
 * \f[ a_1 t^4 + a_2 t^3 + a_3 t^2 + a_4 t + a_5 = 0\f]
 * 
 * where
 *
  \f[ a_1 = d_x^4 + \left(2d_y^2 + 2d_z^2\right)d_x^2 + \left(d_y^4 + 2d_z^2d_y^2 + d_z^4\right) \f]
  \f[ a_2 = \left(4d_x^3 + \left(4d_y^2 + 4d_z^2\right)d_x\right)p_x + \left(\left(4d_yp_y + 4d_zp_z\right)d_x^2 + \left(\left(4d_y^3 + 4d_z^2d_y\right)p_y + \left(4d_zp_zd_y^2 + 4d_z^3p_z\right)\right)\right) \f]
  \f[ a_3 = \left(-2d_x^2 + \left(-2d_y^2 + 2d_z^2\right)\right)R^2 + \left(\left(-2d_x^2 + \left(-2d_y^2 - 2d_z^2\right)\right)r^2 + \left(\left(6d_x^2 + \left(2d_y^2 + 2d_z^2\right)\right)p_x^2 + \left(8d_yp_y + 8d_zp_z\right)d_xp_x + \left(\left(2p_y^2 + 2p_z^2\right)d_x^2 + \left(\left(6d_y^2 + 2d_z^2\right)p_y^2 + 8d_zp_zd_yp_y + \left(2p_z^2d_y^2 + 6d_z^2p_z^2\right)\right)\right)\right)\right) \f]
  \f[ a_4 = \left(-4d_xp_x + \left(-4d_yp_y + 4d_zp_z\right)\right)R^2 + \left(\left(-4d_xp_x + \left(-4d_yp_y - 4d_zp_z\right)\right)r^2 + \left(4d_xp_x^3 + \left(4d_yp_y + 4d_zp_z\right)p_x^2 + \left(4p_y^2 + 4p_z^2\right)d_xp_x + \left(4d_yp_y^3 + 4d_zp_zp_y^2 + 4p_z^2d_yp_y + 4d_zp_z^3\right)\right)\right) \f]
  \f[ a_5 = R^4 + \left(-2r^2 + \left(-2p_x^2 + \left(-2p_y^2 + 2p_z^2\right)\right)\right)R^2 + \left(r^4 + \left(-2p_x^2 + \left(-2p_y^2 - 2p_z^2\right)\right)r^2 + \left(p_x^4 + \left(2p_y^2 + 2p_z^2\right)p_x^2 + \left(p_y^4 + 2p_z^2p_y^2 + p_z^4\right)\right)\right) \f]
 */
class Torus : public Solid, public Transformer {

    public:
	/// Constructor
	Torus(double R, double r, const Material* m);

	virtual ~Torus() {};

	virtual void transform(const Matrix& m);

	virtual AABox getBoundingBox() const;

	virtual SceneObject* clone() const;

	void allIntersections(const Ray& ray, vector<Intersection>& result) const;

        bool inside(const Vector& point) const;

    private:
	double _fastIntersect(const Ray& ray) const;
	void _fullIntersect(const Ray& ray, const double t, Intersection& result) const;
	Vector normal(const Vector& point) const;
	uint32_t allPositiveRoots(const Ray& world_ray, double roots[4]) const;

	double r;
	double R;
};

#endif
