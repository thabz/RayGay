
#include "halton.h"
#include "constants.h"
#include <cstdlib>
#include <iostream>
#include <cassert>    

/**
 * Constructor.
 *
 * @param base a base number
 * @param dim dimension of the sequence
 */
Halton::Halton(int base, int dim) {
    assert(dim > 0);
    invBase = new double[dim];
    values = new double[dim];
    this->dim = dim;
    double v = 0;//RANDOM(0,1);
    for (int i = 0; i < dim; i++) {
	values[i] = v;
	invBase[i] = 1.0 / double(base);
	base = nextPrime(base);
    }
    pthread_mutex_init(&mutex,NULL);
}

Halton::~Halton() {
    delete [] invBase;
    delete [] values;
}

void Halton::reset(double v) {
    for (int i = 0; i < dim; i++)
	values[i] = v;
}

double* Halton::getNext() {
    pthread_mutex_lock(&mutex);
    for (int i = 0; i < dim; i++) {
	double r = 1.0 - values[i] - 1e-10;
	if (invBase[i] < r)
	    values[i] += invBase[i];
	else {
	    double hh;
	    double h = invBase[i];
	    do {
		hh = h;
		h *= invBase[i];
	    } while (h >= r);
	    values[i] += ((hh + h) - 1.0);
	}
    }
    pthread_mutex_unlock(&mutex);
    return values;
}

int Halton::nextPrime(int p) const {
    p = p + (p & 1) + 1;
    while (true) {
	int div = 3;
	bool isPrime = true;
	while (isPrime && ((div * div) <= p)) {
	    isPrime = ((p % div) != 0);
	    div += 2;
	}
	if (isPrime)
	    return p;
	p += 2;
    }
}
