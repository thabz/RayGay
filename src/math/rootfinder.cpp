
#include <cmath>
#include "math/rootfinder.h"

/*
RootFinder::RootFinder(Method method, double tolerance, double (*function) (double)) {
    this->method = method;
    this->f = function;
    this->tolerance = tolerance;
}
*/

/**
 * Constructor.
 */
RootFinder::RootFinder(Method method, double tolerance, Function<double,double>* f) {
    this->method = method;
    this->function = f;
    this->tolerance = tolerance;
}

/**
 * Finds root in the interval \f$ [t1,t2] \f$.
 */
bool RootFinder::solve(double t1, double t2, double* root) {
    switch(method) {
	case BISECTION: 
	    return bisection(t1, t2, root);
	case BRENTS_METHOD: 
	    return brents_method(t1, t2, root);
	case FALSE_POSITION: 
	    return false_position(t1, t2, root);
        default: 
	    return false;			    
    }
}

/**
 * Rootfinding by interval bisection.
 *
 * Brent's method is faster though.
 */
bool RootFinder::bisection(double t_begin, double t_end, double* root) {
    double t_mid = 0.5 * (t_begin + t_end);
    double f_t_begin = f(t_begin);
    double f_t_end = f(t_end);
    double f_t_mid = f(t_mid);

    if (SAME_SIGN(f_t_begin, f_t_end))
	return false;
    
    while (true) {
	if (SAME_SIGN(f_t_begin, f_t_mid)) {
	    t_begin = t_mid;
	    f_t_begin = f_t_mid;
	} else {
	    t_end = t_mid;
	    f_t_end = f_t_mid;
	}

	t_mid = 0.5 * (t_begin + t_end);
	f_t_mid = f(t_mid);

	if (fabs(f_t_mid) < tolerance) {
	    *root = t_mid;
	    return true;
	}
    }
    return false;
}


#define MAX_ITER 1000
/**
 * Brent's method is a root-finding algorithm which combines root 
 * bracketing, interval bisection, and inverse quadratic interpolation. 
 * It is sometimes known as the van Wijngaarden-Deker-Brent method.
 *
 * @see http://mathworld.wolfram.com/BrentsMethod.html
 */
bool RootFinder::brents_method(double x1, double x3, double* root) {
    double x2;
    double R,S,T;
    double P,Q;
    double fx1,fx2,fx3;

    x2 = 0.5 * (x1+x3);

    fx1 = f(x1);
    fx2 = f(x2);
    fx3 = f(x3);

    if (SAME_SIGN(fx1, fx3))
	return false;

    uint i = 0;
    while(i < MAX_ITER) {
	
	R = fx2 / fx3;
	S = fx2 / fx1;
	T = fx1 / fx3;
	
	P = S*(R*(R-T)*(x3-x2)-(1-R)*(x2-x1));
	Q = (T-1)*(R-1)*(S-1);

	x2 = x2 + (P / Q);

	fx2 = f(x2);
	if (fabs(fx2) < tolerance) {
	    *root = x2;
	    return true;
	}
	i++;
    }
    return false;
}

/**
 * Method of False Position.
 *
 * This code is untested!
 *
 * @see http://mathworld.wolfram.com/MethodofFalsePosition.html
 */

bool RootFinder::false_position(double t1, double t2, double* root) {
    double xn1 = t1;
    double xn = t2;
    int i = 0;
    while (fabs(f(xn)) < tolerance || i++ >= MAX_ITER) {
	xn = t1 - ((xn1 - t1) / (f(xn1) - f(t1)))*f(t1);
	xn1 = xn;
    }
    return xn;
}

