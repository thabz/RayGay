
#ifndef PARSER_LIGHTSOURCE_FACTORY_H
#define PARSER_LIGHTSOURCE_FACTORY_H

#include "scheme/scheme.h"

/**
 * Factory for lightsource-related Scheme-procedures.
 */
class LightsourceFactory {
    public:
	static void register_procs(Scheme* scheme);
};

#endif
