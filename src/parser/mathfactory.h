
#ifndef PARSER_MATH_FACTORY
#define PARSER_MATH_FACTORY

#include <libguile.h>

/**
 * Factory for math-related Scheme-procedures.
 */
class MathFactory 
{
    public:
	static void register_procs();
};

#endif

