
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

void BindingEnvironment::put(string name, SchemeObject* o) {
    binding_map[name] = o;
}
