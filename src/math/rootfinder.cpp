
#include <cmath>
#include "math/rootfinder.h"

RootFinder::RootFinder(double t1, double t2, RootFinder::Method method, double tolerance, double (*function) (double)) {
    this->t1 = t1;
    this->t2 = t2;
    this->method = method;
    this->f = function;
    this->tolerance = tolerance;
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

inline
int sign(double val) {
    return val >= 0 ? 1 : -1;
}

#define MAX_ITER 100
/**
 * Brent's method is a root-finding algorithm which combines root 
 * bracketing, interval bisection, and inverse quadratic interpolation. 
 * It is sometimes known as the van Wijngaarden-Deker-Brent method.
 * @see http://mathworld.wolfram.com/BrentsMethod.html
 */
bool RootFinder::brents_method(double* root) {
    double x1,x2,x3;
    double R,S,T;
    double P,Q;
    double fx1,fx2,fx3;
    double x;

    x1 = t1;
    x2 = (t1+t2) / 2.0;
    x3 = t2;
    if (sign(f(x1)) == sign(f(x3)))
	return false;

    while(true) {
	fx1 = f(x1); fx2 = f(x2); fx3 = f(x3);
	
	R = fx2 / fx3;
	S = fx2 / fx1;
	T = fx1 / fx3;
	
	P = S*(R*(R-T)*(x3-x2)-(1-R)*(x2-x1));
	Q = (T-1)*(R-1)*(S-1);

	x = x2 + (P / Q);

	if (fabs(f(x)) < tolerance)
	    return x;

    }
    return false;
}

