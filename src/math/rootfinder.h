
#ifndef MATH_ROOT_FINDER_H
#define MATH_ROOT_FINDER_H

#include "math/function.h"

/**
 * A collection of rootfinding algorithms.
 */
class RootFinder {
    public:
	enum Method {
	    BISECTION,
	    BRENTS_METHOD,
	    REGULA_FALSI
	};

	RootFinder(Method method, double tolerance, Function<double,double>* f); 
	int solve(double t1, double t2, double* root);

    protected:
	int bisection(double t1, double t2, double* root);
	int brents_method(double t1, double t2, double* root);
	int regula_falsi(double t1, double t2, double* root);


    private:
	double f(double t) const;

	double tolerance;
	RootFinder::Method method;
	Function<double,double>* function;
};

inline
double RootFinder::f(double t) const {
    return function->eval(t);
}

#endif

