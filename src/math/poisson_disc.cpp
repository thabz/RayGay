
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


void PoissonDiscDistribution::boundarySampling(double w, double h, double r, uint32_t num) {
    vector<ArcInterval> boundaries;
    uint32_t i = 0;
    double k  = RANDOM(0,10);

    Vector2 initial_point = Vector2(RANDOM(0,w),RANDOM(0,h));
    boundaries.push_back(ArcInterval(initial_point,2*r));
    add(initial_point, 2*r);
	
    while((!boundaries.empty()) && i++ < num) {
	Vector2 p = boundaries[0].randomPoint();
	ArcInterval new_boundary = ArcInterval(p,2*r);

	// Subtract the circle at p's from all other active boundaries. Also prune the empty.
	for(int j = boundaries.size()-1; j >= 0; j--) {
    	    boundaries[j].subtract(p, 2*r);
	    if (boundaries[j].isEmpty()) {
		boundaries.erase(boundaries.begin()+j);
	    }
	}

	// Subtract all neighbouring boundaries from the boundary around p
	vector<Vector2>& neighbours = getNeighbours(p);
	for(uint32_t j = 0; j < neighbours.size(); j++) {
	    new_boundary.subtract(neighbours[j], 2*r);
	}
	
	// Subtract the box [0,w]x[0,h] from new boundary
	new_boundary.subtract(Vector2(0,0), Vector2(w,h));

	// Store new boundary 
	if (!new_boundary.isEmpty()) {
   	    boundaries.push_back(new_boundary);
        }
        
	add(p, 2*r);
	
//	cout << "Boundaries: " << boundaries.size() << endl;
    }
}

