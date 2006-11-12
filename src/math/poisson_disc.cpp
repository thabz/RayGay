
#include "math/poisson_disc.h"
#include "math/interval.h"
#include "math/vector.h"

#include <cassert>
#include <cstdlib>
#include "constants.h"
#include <iostream>
#include <vector>

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


class Boundary {
    public:
	Boundary(Vector2 c, double r) {
	    this->c = c;
	    this->r = r;
	    interval = Interval(0.0, M_2PI);
	}

	bool isEmpty() { return interval.isEmpty(); };

	const Vector2& getC() { return c; };
	double getR() { return r; };
	
	Vector2 random() {
	    assert(!isEmpty());
	    double angle = interval.random();
	    return c + r * Vector2(cos(angle),sin(angle));
	}

	// Subtract a circle
	void subtract(const Vector2& o_c, double o_r);
	
	// Subtract an 2D axis aligned box
	void subtract(const Vector2& lower, const Vector2& upper) {
	   // TODO: implement     
	}

    private:
	Vector2 c;
	Interval interval;
	double r;
};

void Boundary::subtract(const Vector2& o_c, double o_r) {
    assert(IS_EQUAL(o_r,r));
    if ((o_c - c).norm() > 2*r*2*r) {
	return;
    }
    
    Vector2 middle = (c + o_c) * 0.5;
    double angle = acos(middle.length() / r);
    angle += acos(middle.length() / o_c[0]); // TODO: x kan være 0.
    double from_angle = 0;
    double to_angle = 0;
    interval.subtract(from_angle,to_angle);
}


int PoissonDiscDistribution::BoundarySampling(double w, double h, double r, uint32_t num, Vector2* result) {
    vector<Boundary> boundaries;
    uint32_t i = 0;

    Vector2 initial_point = Vector2(RANDOM(0,w),RANDOM(0,h));
    boundaries.push_back(Boundary(initial_point,2*r));
    result[i++] = initial_point;
	
    while(! boundaries.empty() && i < num) {
	Vector2 p = boundaries[0].random();
	result[i++] = p;
	Boundary new_boundary = Boundary(p,2*r);

	// Subtract the circle at p's from all other boundaries and vise-versa.
	for(uint32_t j = 0; j < boundaries.size(); j++) {
	    new_boundary.subtract(boundaries[j].getC(), boundaries[j].getR());
	    boundaries[j].subtract(new_boundary.getC(), new_boundary.getR());
	}
	
	// Subtract the box [0,w]x[0,h] from new boundary
	new_boundary.subtract(Vector2(0,0), Vector2(w,h));

	// Store new bounary 
	boundaries.push_back(new_boundary);
	
	// Delete empty boundaries and their corresponding centers
	for(uint32_t j = 0; j < boundaries.size(); j++) {
	    if (boundaries[j].isEmpty()) {
		boundaries.erase(boundaries.begin()+j);
	    }
	}
    }
    return i;
}

