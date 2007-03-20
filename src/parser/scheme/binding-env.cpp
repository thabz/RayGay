
#include "binding-env.h"

BindingEnvironment::BindingEnvironment(BindingEnvironment* parent) {
    this->parent = parent;
}

SchemeObject* BindingEnvironment::get(string name) {
    map<string,SchemeObject*>::iterator v = binding_map.find(name);
    if (v == binding_map.end()) {
        if (parent != NULL) {
            return parent->get(name);
        } else {
            return NULL;
        }
    } else {
        return v->second;
    }
}

SchemeObject* BindingEnvironment::get(SchemeSymbol* name) {
    return get(name->str);
}


void BindingEnvironment::put(string name, SchemeObject* o) {
    binding_map[name] = o;
}

void BindingEnvironment::put(SchemeSymbol* name, SchemeObject* o) {
    put(name->str, o);
}
