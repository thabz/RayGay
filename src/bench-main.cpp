
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdlib>
#include <ctime>
#include <cstdio>
#include <iostream>
#include <vector>
#include <unistd.h>
#include <time.h>

#include "math/vector.h"

using namespace std;

Vector random_vectors[1024];
double random_doubles[1024];

void bench_vector() {
    Vector one = Vector(1,1,1);
    Vector two = Vector(2,2,2);
    Vector sum = Vector(0,0,0);
    for(ulong i = 0; i < 500000000; i++) {
	sum += random_vectors[i & 1023];
	sum = sum + one;
	sum = sum + two * 4.0;
	sum = sum + two / 4.0;
	for (uint j = 0; j < 20; j++) {
	    sum = sum + sum / 3.0;
	    sum = sum + sum / 8.0;
	    sum = sum + sum / 0.5;
	}
	sum *= random_doubles[i & 1023];
    }
}

void bench(std::string name, void (*func)(void), uint repeat_num) {
    cout << "Running benchmark \"" << name << "\"" << endl;
    double min = 100000;
    double max = -1;
    double avg = 0;
    clock_t ticks_before;
    clock_t ticks_after;
    for(uint i = 0; i < repeat_num; i++) {
	ticks_before = clock();
	func();
	ticks_after = clock();
	double secs =  double(ticks_after - ticks_before) / double(CLOCKS_PER_SEC);
	cout << "   " << (i+1) << ": " << secs << "s" << endl;
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
    for(uint i = 0; i < 1000; i++) {
	random_doubles[i] = RANDOM(-1,1);
	random_vectors[i] = Vector(RANDOM(-1,1),RANDOM(-1,1),RANDOM(-1,1));
    }
}

int main(int argc, char *argv[]) {

    cout << "Clocks/s : " << CLOCKS_PER_SEC << endl;
    init();
    bench("Vector", bench_vector, 5);
    return EXIT_SUCCESS;
}






