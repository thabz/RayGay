
#ifndef PATH_SUPERELLIPSE_H
#define PATH_SUPERELLIPSE_H

/**
 * The superellipse path. The curve has the Cartesian equation
 *
 * \f[ \left | \frac{x}{a} \right |^r + \left | \frac{y}{b} \right |^r = 1 \f]
 * 
 * The curve is described parametrically as
 *
 * \f[ F(t) = ( a\cos^{2/r}t, b\sin^{2/r}t ) \f]
 *
 * It's tangent vector becomes
 *
 * \f[ F'(t) = ( -\frac{2a}{r} \cos^{2/r - 1}t \sin t, \frac{2b}{r} \sin^{2/r - 1}t \cos t \f]
 *
 * @see http://mathworld.wolfram.com/Superellipse.html
 */
class SuperEllipse : public Path 
{
    public:

	SuperEllipse(const Vector& center, double a, double b, double r, const Vector& normal);
	virtual ~SuperEllipse() {};
	Vector getPoint(double t) const;
	Vector getTangent(double t) const;
	void transform(const Matrix& m);

    private:
	Vector c;
	Vector n;
	double r;
	double a,b;
	Matrix m;
	Matrix orient;

};

#endif
