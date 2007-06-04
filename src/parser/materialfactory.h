
#ifndef PARSER_MATERIAL_FACTORY_H
#define PARSER_MATERIAL_FACTORY_H

#include "scheme/scheme.h"

class MaterialFactory {
    public:
	static SchemeObject* make_material(SchemeObject* options);
	static void register_procs(Scheme* scheme);
    private:
        static Scheme* scheme;    	
};

#endif
