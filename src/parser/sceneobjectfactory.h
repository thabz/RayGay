
#ifndef PARSER_SCENEOBJECT_FACTORY_H
#define PARSER_SCENEOBJECT_FACTORY_H

#include <libguile.h>

/**
 * Factory for sceneobject-related Scheme-procedures.
 */
class SceneObjectFactory {
    public:
	static void register_procs();
};

#endif
