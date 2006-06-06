
#include "math/poisson_disc.h"
#include <cassert>
#include <cstdlib>
#include "constants.h"
#include <iostream>

using namespace std;

#define MAX_TRIES_PER_DART 1000000
int PoissonDiscDistribution::createSet(double w, double h, double r, int num, Vector2* result)
{
    assert(num > 0);
    assert(r > 0.0);
    Vector2 dart;
    bool found_one, dart_ok;
    int i = 0, j, tries;
    double min_dist_sqr = 2 * r * 2 * r;
    do {
	tries = 0;
	found_one = false;
	while (tries < MAX_TRIES_PER_DART && !found_one) {
	    dart = Vector2(RANDOM(0,w), RANDOM(0,h));
	    dart_ok = true;
	    for(j = 0; j < i && dart_ok; j++) {
		double dist_sqr = (dart - result[j]).norm();
		if (dist_sqr <= min_dist_sqr) {
		    dart_ok = false;
		}
	    }
	    if (dart_ok) {
		result[i++] = dart;
		found_one = true;
	    } else {
		tries++;
	    }
	}
	//cout << "Tries: " << tries << endl;
    } while(i < num && found_one);
    return i;
}

/*

class Boundary {
    public:
	Boundary(Vector2 c, double r) {
	    this->c = c;
	    this->r = r;
	    num = 1;
	    ranges[0] = 0.0;
	    ranges[1] = M_2PI;
	}
	bool hasAvailableBoundary() { return num == 0; };
	Vector2 getRandomPointOnBoundary() {
	    assert(hasAvailableArc());
	    int i = 0; // TODO: Pick a random range instead of the first
	    double angle = RANDOM(ranges[i*2+0],ranges[i*2+1]);
	    return c + r * Vector2(cos(angle),sin(angle));
	}
	void pruneAngularRanges(Vector2 c_o, double r_o) {
	    assert(IS_EQUAL(r_o,r));
	    Vector3 middle = (c + c_o) * 0.5;
	    double angle = acos(middle.length() / r);
	    angle += acos(middle.length() / c_o.x()); // TODO: x kan være 0.

	}
    private:
	Vector2 c;
	double ranges[2*10];
	double r;
	int num;
};

int PoissonDiscDistribution::BoundarySampling(double w, double h, double r, int num, Vector2* result) {
    ArcSegment* arc_segments = (ArcSegment*)alloca(sizeof(ArcSegment) * num * 10); 
    bool done = false;
    int i = 0;
    while (!done) {
	dart = Vector2(RANDOM(0,w), RANDOM(0,h));
	result[0] = dart;
	arc_segments[0*10] = ArcSegment(0,2*M_PI);
    } 
    return i;
}

*/

/*
int PoissonDiscDistribution::BoundarySampling(double w, double h, double r, int num, Vector2* result) {
    vector<Interval> boundaries;
    vector<Vector2> centers;
    uint32_t i = 0;

    Vector2 initial_point = Vector2(RANDOM(0,w),RANDOM(0,h));
    centers.push_back(initial_point);
    boundaries.push_back(Interval(0,2*M_PI));
    result[i++] = initial_point;
	
    while(! boundaries.empty() && i < num) {
	double angle = boundaries[0].random();
	Vector2 p = centers[0] + 2*r*Vector2(cos(angle),sin(angle));
	result[i++] = p;

	// Subtract the box [0,w]x[0,h] from p's boundary

	// Subtract p's boundary from all other boundaries and vise-versa.

	// push_back p's boundary to boundaries
	// push_back p to centers

	// Delete empty boundaries and their corresponding centers
    }
    return i;
}
*/
