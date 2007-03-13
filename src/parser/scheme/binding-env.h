
#ifndef SCHEME_BINDING_ENV_H
#define SCHEME_BINDING_ENV_H

#include "objects.h"
#include <map>

using namespace std;

class Binding {
    public:
        enum Type {
            SIMPLE,
            MACRO,
            PROCEDURE
        };
     
        Binding::Type type;
        
        // Used for type SIMPLE
        SchemeObject* obj;
};


class BindingEnvironment {
    public:
		BindingEnvironment(BindingEnvironment* parent);
		Binding* get(string name);
        void put(string name, SchemeObject* o);
        
	private:
        BindingEnvironment* parent;
        map<string,Binding*> binding_map;	
};
	
#endif
