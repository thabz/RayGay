
#ifndef MATH_ROOT_FINDER_H
#define MATH_ROOT_FINDER_H

class RootFinder {
    public:
	enum Method {
	    BISECTION,
	    BRENTS_METHOD,
	    FALSE_POSITION
	};

	RootFinder(Method method, double tolerance, double (*function) (double)); 
	bool solve(double t1, double t2, double* root);

    protected:
	bool bisection(double t1, double t2, double* root);
	bool brents_method(double t1, double t2, double* root);
	bool false_position(double t1, double t2, double* root);


    private:
	double tolerance;
	RootFinder::Method method;
	double (*f) (double);
};

#endif

