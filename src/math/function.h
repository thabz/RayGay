
#ifndef MATH_FUNCTION_H
#define MATH_FUNCTION_H

#include "math/vector.h"
#include "types.h"

/**
 * A template for functions.
 *
 * Examples:
 *
 * Function<double,double> specifies a function \f$ \Re \rightarrow \Re \f$.
 * 
 * Function<Vector,double> specifies a function \f$ \Re^3 \rightarrow \Re \f$
 * that could be used as a density function.
 *
 * Function<Vector,Vector> specifies a function \f$ \Re^3 \rightarrow \Re^3 \f$
 * that could be used as a normal-transformation.
 *
 */
template<typename FROM, typename TO> 
class Function {
    public:
	/// Evaluate the function.
	virtual TO eval(const FROM& x) const = 0;

	/// Shortcut to evaluate function.
	TO operator()(const FROM& x) const {
	    return eval(x);
	}
};

#endif

