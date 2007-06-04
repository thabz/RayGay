
#ifndef PARSER_MATERIAL_FACTORY_H
#define PARSER_MATERIAL_FACTORY_H

#include <libguile.h>

class MaterialFactory {
    public:
	static SCM make_material(SCM options);
	static void register_procs(Scheme* scheme);
};

#endif
