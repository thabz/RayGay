
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdlib>
#include <ctime>
#include <cstdio>
#include <iostream>
#include <vector>
#include <cassert>

extern "C" {
#include <unistd.h>
#include <time.h>
}

#include "math/vector.h"
#include "objects/ellipsoid.h"
#include "math/poisson_disc.h"
#include "ray.h"
#include "exception.h"
#include "ttf.h"
#include "gzip.h"

using namespace std;

Vector random_vectors[1024];
double random_doubles[1024];

void bench_ellipsoid_fast_intersect() {
    // Make some rays and some ellipsoids
    Ellipsoid* ellipsoids[1024];
    Ray* rays[1024];
    uint32_t num = 5000000;
    for(uint32_t i = 0; i < 1024; i++) {
	Ellipsoid* e = new Ellipsoid(random_vectors[i],
		                     Vector(RANDOM(2,3),RANDOM(2,3),RANDOM(2,3)),
					 NULL);
	ellipsoids[i] = e;

	Vector from =  random_vectors[i] * 1000;
	Vector dir = random_vectors[i] - from;
	Ray* ray = new Ray(from,dir,0);
	rays[i] = ray;
    }

    double result = 0;
    for(uint32_t i = 0; i < num; i++) {
	result += ellipsoids[i & 1023]->fastIntersect(*rays[i & 1023]);
    }
    cout << result << endl;
}

void bench_vector() {
    Vector one = Vector(1,1,1);
    Vector two = Vector(2,2,2);
    Vector sum = Vector(0,0,0);
    for(long i = 0; i < 500000000; i++) {
	sum += random_vectors[i & 1023];
	sum = sum + one;
	sum = sum + two * 4.0;
	sum = sum + two / 4.0;
	for (uint32_t j = 0; j < 20; j++) {
	    sum = sum + sum / 3.0;
	    sum = sum + sum / 8.0;
	    sum = sum + sum / 0.5;
	}
	sum *= random_doubles[i & 1023];
    }
}

void bench_poisson_disc() {
    int samples = 1000;
    uint32_t repeats = 480*100;
    Vector2* result = new Vector2[samples];
    PoissonDiscDistribution distr = PoissonDiscDistribution(10,10);
    for(uint32_t i = 0; i < repeats; i++) {
        int num = distr.createSet(0.147,samples,result);
        assert(num > 900);
    }
}

void bench(std::string name, void (*func)(void), uint32_t repeat_num) {
    cout << "Running benchmark \"" << name << "\"" << endl;
    double min = 100000;
    double max = -1;
    double avg = 0;
    clock_t ticks_before;
    clock_t ticks_after;
    for(uint32_t i = 0; i < repeat_num; i++) {
	ticks_before = clock();
	func();
	ticks_after = clock();
	double secs =  double(ticks_after - ticks_before) / double(CLOCKS_PER_SEC);
	cout << "   " << (i+1) << ": " << secs << "s" << endl << flush;
	min = MIN(secs,min);
	max = MAX(secs,max);
	avg += secs;
    }
    avg /= repeat_num;
    cout << "min: " << min << "s" << endl;
    cout << "max: " << max << "s" << endl;
    cout << "avg: " << avg << "s" << endl;
}

void init() {
    // Make sure rand is seeded consistently.
    srand(1); 
    for(uint32_t i = 0; i < 1000; i++) {
	random_doubles[i] = RANDOM(-1,1);
	random_vectors[i] = Vector(RANDOM(-1,1),RANDOM(-1,1),RANDOM(-1,1));
    }
}

void ttf_test() {
    TrueTypeFont* arial = new TrueTypeFont("arial.ttf");        
    assert(arial != NULL);
}

void gzip_test() {
    GZIP* gzip = new GZIP("test.gz");        
    gzip->deflate();
//    gzip->dump_buffer();
}

int main(int argc, char *argv[]) {
    try {
//        cout << "Clocks/s : " << CLOCKS_PER_SEC << endl;
        init();
        //bench("Vector", bench_vector, 5);
        //bench("Ellipsoid fast intersect", bench_ellipsoid_fast_intersect, 5);
        //bench("Poisson disc", bench_poisson_disc, 5);
        gzip_test();
    } catch (Exception e) {
    	cout << "Exception: " << e.getMessage() 
     	     << " at " << e.getSourceFile() << ":" << e.getSourceLine() << endl;
    	return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}
