
#include "math/poisson_disc.h"
#include <cassert>
#include <cstdlib>
#include "constants.h"

#define MAX_TRIES_PER_DART 100
int PoissonDiscDistribution::createSet(double w, double h, double r, int num, Vector2* result)
{
    assert(num > 0);
    Vector2 dart;
    bool found_one = true;
    int i = 0, j, tries;
    double rr = r * r;
    do {
	tries = 0;
	found_one = false;
	while (tries < MAX_TRIES_PER_DART && !found_one) {
	    dart = Vector2(RANDOM(0,w), RANDOM(0,h));
	    bool miss = false;
	    for(j = 0; j < i && !miss; j++) {
		double dist_sqr = (dart - result[j]).norm();
		if (dist_sqr < rr) {
		    miss = true;
		}
	    }
	    if (miss) {
		tries++;
	    } else {
		result[++i] = dart;
	    }
	}
    } while(i < num && found_one);
    return i;
}

