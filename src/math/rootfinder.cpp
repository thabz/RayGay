
#include "math/rootfinder.h"

RootFinder::RootFinder(double t1, double t2, RootFinder::Method method, double (*function) (double)) {
    this->t1 = t1;
    this->t2 = t2;
    this->method = method;
    this->f = function;
}

bool RootFinder::solve(double* root) {
    switch(method) {
	case BISECTION: 
	    return bisection(root);
	case BRENTS_METHOD: 
	    return brents_method(root);
        default: 
	    return false;			    
    }
}

bool RootFinder::bisection(double* root) {
    return false;
}

bool RootFinder::brents_method(double* root) {
    return false;

}

