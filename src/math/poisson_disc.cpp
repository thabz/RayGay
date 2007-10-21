
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

PoissonDiscDistribution::PoissonDiscDistribution(double w, double h) {
    this->w = w;
    this->h = h;
}

int PoissonDiscDistribution::createSet(double r, uint32_t num, Vector2* result) {
    boundarySampling(r,num);
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
void PoissonDiscDistribution::dartThrowing(double r, uint32_t num)
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

/**
 * Implementing "A Spatial Data Structure for Fast Poisson-Disk Sample Generation" by
 * Daniel Dunbar and Greg Humphreys, University of Virginia. Proceedings of SIGGRAPH 2006.
 * 
 * @see http://www.cs.virginia.edu/~gfx/pubs/antimony/
 */
void PoissonDiscDistribution::boundarySampling(double r, uint32_t num) {

    vector<ArcInterval> boundaries;
    boundaries.reserve(num);
    uint32_t i = 0;
    Vector2 p; 

    do {
        if (i == 0) {
            // Initial seed point        
            p = Vector2(RANDOM(0,w),RANDOM(0,h));                
        } else {
            p = boundaries[rand() % boundaries.size()].randomPoint();        
        }     
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

        /*
	for(uint32_t j = 0; j < boundaries.size(); j++) {
	    assert(!boundaries[j].isEmpty());
        }
        */
	
	//cout << "Boundaries: " << boundaries.size() << endl;
    } while ((!boundaries.empty()) && i++ < num);
}

