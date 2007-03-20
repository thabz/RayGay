
#ifndef SCHEME_BINDING_ENV_H
#define SCHEME_BINDING_ENV_H

#include "objects.h"
#include <map>

using namespace std;

class BindingEnvironment {
    public:
		BindingEnvironment(BindingEnvironment* parent);
		SchemeObject* get(SchemeSymbol* name);
        void put(SchemeSymbol* name, SchemeObject* o);
        
	private:
        BindingEnvironment* parent;
        map<SchemeSymbol*,SchemeObject*> binding_map;	
};
	
#endif
