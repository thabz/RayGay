
#ifndef MATH_ROOT_FINDER_H
#define MATH_ROOT_FINDER_H

class RootFinder {
    public:
	enum Method {
	    BISECTION,
	    BRENTS_METHOD,
	    FALSE_POSITION
	};

	RootFinder(double t1, double t2, RootFinder::Method method, double tolerance, double (*function) (double)); 
	bool solve(double* root);

    protected:
	bool bisection(double* root);
	bool brents_method(double* root);
	bool false_position(double* root);


    private:
	double t1, t2;
	double tolerance;
	RootFinder::Method method;
	double (*f) (double);
};

#endif

