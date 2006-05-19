
#include "math/poisson_disc.h"
#include <cassert>
#include <cstdlib>
#include "constants.h"
#include <iostream>

using namespace std;

#define MAX_TRIES_PER_DART 100000
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

