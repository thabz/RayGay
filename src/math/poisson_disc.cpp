
#include "math/poisson_disc.h"
#include "math/interval.h"
#include "math/vector.h"
#include "exception.h"

#include <cassert>
#include <cstdlib>
#include "constants.h"
#include <iostream>
#include <vector>
#include <list>

using namespace std;

vector<Vector2> PoissonDiscDistribution::all;

int PoissonDiscDistribution::createSet(double w, double h, double r, uint32_t num, Vector2* result) {
    boundarySampling(w,h,r,num);
    for(uint32_t i = 0; i < num && i < all.size(); i++) {
         result[i] = all[i];            
    }
    return MIN(num,all.size());
}

vector<Vector2>& PoissonDiscDistribution::getNeighbours(const Vector2& p) {
    return all;        
}

void PoissonDiscDistribution::add(const Vector2& p, double r) {
    all.push_back(p);        
}

#define MAX_TRIES_PER_DART 10000000
void PoissonDiscDistribution::dartThrowing(double w, double h, double r, uint32_t num)
{
    assert(num > 0);
    assert(r > 0.0);
    Vector2 dart;
    bool found_one, dart_ok;
    uint32_t i = 0, j, tries;
    double min_dist_sqr = 2 * r * 2 * r;
    do {
	tries = 0;
	found_one = false;
	while (tries < MAX_TRIES_PER_DART && !found_one) {
	    dart = Vector2(RANDOM(0,w), RANDOM(0,h));
	    dart_ok = true;
	    vector<Vector2>& neighbours = getNeighbours(dart);
	    for(j = 0; j < neighbours.size() && dart_ok; j++) {
		double dist_sqr = (dart - neighbours[j]).norm();
		if (dist_sqr <= min_dist_sqr) {
		    dart_ok = false;
		}
	    }
	    if (dart_ok) {
		add(dart,2*r);
		found_one = true;
	    } else {
		tries++;
	    }
	}
	//cout << "Tries: " << tries << endl;
    } while(i < num && found_one);
}


Boundary::Boundary(Vector2 c, double r) {
    this->c = c;
    this->r = r;
    interval = Interval(0.0, M_2PI);
}

bool Boundary::contains(double angle) const {
    return interval.contains(angle);        
};


Vector2 Boundary::random() const {
    assert(!isEmpty());
    double angle = interval.random();
    return c + r * Vector2(cos(angle),sin(angle));
}

void Boundary::subtract(const Vector2& o_c, double o_r) {
    Vector2 v = o_c - c;
    if (v.norm() < (r+o_r)*(r+o_r)) {
        double l = v.length();
        double angle = atan2(v[1],v[0]);
        double theta = acos((r-(r+o_r-l)/2.0)/r);
   
        subtract(angle - theta, angle + theta);
    }
}

#define get_angle(d,r) acos(fabs(d)/(r))
void Boundary::subtract(const Vector2& lower, const Vector2& upper) {
    double angle, theta;
    if (c[0] + r > upper[0]) {
        angle = 0.0 * M_PI;    
//        cout << "Box subtract 0" << endl;
        theta = get_angle(upper[0]-c[0],r);
        subtract(angle-theta,angle+theta);
    }
    if (c[0] - r < lower[0]) {
        angle = 1.0 * M_PI;    
//            cout << "Box subtract 1" << endl;
        theta = get_angle(c[0]-lower[0],r);
        subtract(angle-theta,angle+theta);
    }
    if (c[1] + r > upper[1]) {
        angle = 0.5 * M_PI;    
//        cout << "Box subtract 2" << endl;
        theta = get_angle(upper[1]-c[1],r);
        subtract(angle-theta,angle+theta);
    }
    if (c[1] - r < lower[1]) {
        angle = 1.5 * M_PI;    
//        cout << "Box subtract 3" << endl;
        theta = get_angle(c[1]-lower[1],r);
        subtract(angle-theta,angle+theta);
    }
}

void Boundary::subtract(double from, double to) {
    if (from > to) swap(from,to);
    if (from >= 0 && to <= M_2PI) {
        interval.subtract(from, to);
    } else if (from < 0 && to < M_2PI) {
        interval.subtract(0,to);
        interval.subtract(M_2PI + from, M_2PI); 
    } else if (to > M_2PI && from > 0) {
        interval.subtract(from,M_2PI);
        interval.subtract(0,to - M_2PI);    
    } else {
        throw_exception("What?");    
    }
}

void PoissonDiscDistribution::boundarySampling(double w, double h, double r, uint32_t num) {
    vector<Boundary> boundaries;
    uint32_t i = 0;
    double k  = RANDOM(0,10);

    Vector2 initial_point = Vector2(RANDOM(r,w-r),RANDOM(r,h-r));
    boundaries.push_back(Boundary(initial_point,2*r));
    add(initial_point, 2*r);
	
    while((!boundaries.empty()) && i++ < num) {
	Vector2 p = boundaries[0].random();
	Boundary new_boundary = Boundary(p,2*r);

	// Subtract the circle at p's from all other active boundaries
	for(uint32_t j = 0; j < boundaries.size(); j++) {
	    boundaries[j].subtract(p, 2*r);
	}

	// Subtract all neighbouring boundaries from the boundary around p
	vector<Vector2>& neighbours = getNeighbours(p);
	for(uint32_t j = 0; j < neighbours.size(); j++) {
	    new_boundary.subtract(neighbours[j], 2*r);
	}
	
	// Subtract the box [0,w]x[0,h] from new boundary
	new_boundary.subtract(Vector2(0,0), Vector2(w,h));

	// Store new boundary 
	boundaries.push_back(new_boundary);
	add(p, 2*r);
	
	// Delete empty boundaries and their corresponding centers
	for(int j = boundaries.size()-1; j >= 0; j--) {
	    if (boundaries[j].isEmpty()) {
		boundaries.erase(boundaries.begin()+j);
	    }
	}
//	cout << "Boundaries: " << boundaries.size() << endl;
    }
}

