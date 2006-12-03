
#ifndef MATH_POISSON_DISC
#define MATH_POISSON_DISC

#include "math/vector2.h"
#include "math/interval.h"
#include <vector>

class PoissonDiscDistribution
{
    public:
	/**
	 * Creates a set of random points within [0,w] &cross; [0,h] that
	 * all are at least a distance of 2r from each other.
	 * 
	 * @param result must have room for num Vector2's
	 * @return the number of points found whis is &leq; num
	 */
	static int createSet(double w, double h, double r, uint32_t num, Vector2* result);

    private:
    	static void dartThrowing(double w, double h, double r, uint32_t num);
	static void boundarySampling(double w, double h, double r, uint32_t num);
	static std::vector<Vector2>& getNeighbours(const Vector2& p);
	static void add(const Vector2& p, double r);
	static std::vector<Vector2> all;
};

class Boundary {
    public:
	Boundary(Vector2 c, double r);

	bool isEmpty() const { return interval.isEmpty(); };

	const Vector2& getC() { return c; };
	double getR() { return r; };
	
	Vector2 random() const ;

	// Subtract a circle
	void subtract(const Vector2& o_c, double o_r);
	
	// Subtract an 2D axis aligned box
	void subtract(const Vector2& lower, const Vector2& upper);
	
	// Subtract an angle segment
	void subtract(double from, double to);
	
	bool contains(double angle) const;

    private:
	Vector2 c;
	Interval interval;
	double r;
};


#endif
