
#include <cmath>
#include "math/rootfinder.h"
#include <float.h>

#define MAX_ITER 100

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
int RootFinder::solve(double t1, double t2, double* root) {
    switch(method) {
	case BISECTION: 
	    return bisection(t1, t2, root);
	case BRENTS_METHOD: 
	    return brents_method(t1, t2, root);
	case REGULA_FALSI: 
	    return regula_falsi(t1, t2, root);
        default: 
	    return false;			    
    }
}


/**
 * Rootfinding by interval bisection.
 *
 * Brent's method is faster though.
 */
int RootFinder::bisection(double t_begin, double t_end, double* root) {
    double t_mid;
    double f_t_begin = f(t_begin);
    double f_t_end = f(t_end);
    double f_t_mid;
    uint i = 2;

    if (SAME_SIGN(f_t_begin, f_t_end))
	return false;
    
    while (i++ < MAX_ITER) {
	t_mid = 0.5 * (t_begin + t_end);
	f_t_mid = f(t_mid);

	if (fabs(f_t_mid) < tolerance) {
	    *root = t_mid;
	    return i;
	}

	if (SAME_SIGN(f_t_begin, f_t_mid)) {
	    t_begin = t_mid;
	    f_t_begin = f_t_mid;
	} else {
	    t_end = t_mid;
	    f_t_end = f_t_mid;
	}

    }
    return false;
}


/**
 * Brent's method is a root-finding algorithm which combines root 
 * bracketing, interval bisection, and inverse quadratic interpolation. 
 * It is sometimes known as the van Wijngaarden-Deker-Brent method.
 *
 * Using the algorithm from Numerical Recipes in FORTRAN.
 *
 * @see http://www.library.cornell.edu/nr/cbookfpdf.html
 */
int RootFinder::brents_method(double x1, double x2, double* root) {
    double a,b,c;
    double r,s;
    double p,q;
    double fa,fb,fc;
    double tol,m;
    bool ac_equal;
    double e,d;

    d = x2 - x1;
    e = d;

    a = x1;
    b = x2;
    c = b;

    fa = f(a);
    fb = f(b);
    fc = f(c);

    if (SAME_SIGN(fa, fb))
	return false;

    uint i = 3;
    while(i++ < MAX_ITER) {
	ac_equal = false;

	if ((fb < 0 && fc < 0) || (fb > 0 && fc > 0)) {
	    ac_equal = true;
	    c = a;
	    fc = fa;
	    d = b - a;
	    e = b - a;
	}

	if (fabs(fc) < fabs(fb)) {
	    ac_equal = true;
	    a = b;
	    b = c;
	    c = a;
	    fa = fb;
	    fb = fc;
	    fc = fa;
	}

	//tol = 0.5 * DBL_EPSILON * fabs (b) + 0.5 * tolerance;
	tol = 0.5 * DBL_EPSILON * fabs (b);
	
	m = 0.5 * (c - b);

	if (fabs(fb) < tolerance) {
	    *root = b;
	    return i;
	}

	/*
	if (fabs(m) < tol) {
	    *root = b;
	    return i;
	}
	*/

	if (fabs(e) < tol || fabs(fa) <= fabs(fb)) {
	    // Using bisection
	    d = m;
	    e = m;
	} else {
	    // Use inverse cubic interpolation
	    s = fb / fa;

	    if (ac_equal) {
		p = 2 * m * s;
		q = 1 - s;
	    } else {
		q = fa / fb;
		r = fb / fc;
		p = s * (2 * m * q * (q - r) - (b - a) * (r - 1));
		q = (q - 1) * (r - 1) * (s - 1);
	    }

	    if (p > 0) {
		q = -q;
	    } else {
		p = -p;
	    }

	    if (2 * p < MIN(3 * m * q - fabs(tol * q), fabs (e * q)))
	    {
		e = d;
		d = p / q;
	    } else {
		// interpolation failed, fall back to bisection
		d = m;
		e = m;
	    }
	}

	a = b;
	fa = fb;

	if (fabs (d) > tol) {
	    b += d;
	} else {
	    //b += (m > 0 ? +tol : -tol);
	    b += copysign(tol,m);
	}

	fb = f(b);
    }
    return false;
}

/**
 * Regula falsi aka method of False Position.
 *
 * @see http://mathworld.wolfram.com/MethodofFalsePosition.html
 */

int RootFinder::regula_falsi(double t_begin, double t_end, double* root) {
    double t_mid;
    double f_t_begin = f(t_begin);
    double f_t_end = f(t_end);
    double f_t_mid;
    uint i = 2;

    if (SAME_SIGN(f_t_begin, f_t_end))
	return false;

    while (i++ < MAX_ITER) {
	t_mid = ( f_t_end * t_begin - f_t_begin * t_end ) / ( f_t_end - f_t_begin );
	f_t_mid = f(t_mid);

	if (fabs(f_t_mid) < tolerance) {
	    *root = t_mid;
	    return i;
	}

	if (SAME_SIGN(f_t_begin, f_t_mid)) {
	    t_begin = t_mid;
	    f_t_begin = f_t_mid;
	} else {
	    t_end = t_mid;
	    f_t_end = f_t_mid;
	}
    }
    return false;
}

