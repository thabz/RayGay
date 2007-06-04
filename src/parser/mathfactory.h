
#ifndef PARSER_MATH_FACTORY
#define PARSER_MATH_FACTORY

#include "scheme/scheme.h"

/**
 * Factory for math-related Scheme-procedures.
 */
class MathFactory 
{
    public:
	static void register_procs(Scheme* scheme);
};

#endif

