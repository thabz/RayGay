
#ifndef PARSER_MATH_FACTORY
#define PARSER_MATH_FACTORY

#include <libguile.h>

/**
 * Factory for math-related Scheme-procedures.
 */
class MathFactory 
{
    public:
	static SCM random(SCM s_min, SCM s_max);
	static void register_procs();
};

#endif

