
#ifndef SCHEME_BINDING_ENV_H
#define SCHEME_BINDING_ENV_H

#include "objects.h"
#include <map>

using namespace std;

class BindingEnvironment {
    public:
		BindingEnvironment(BindingEnvironment* parent);
		SchemeObject* get(string name);
        void put(string name, SchemeObject* o);
        
	private:
        BindingEnvironment* parent;
        map<string,SchemeObject*> binding_map;	
};
	
#endif
