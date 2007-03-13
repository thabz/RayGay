
#include "binding-env.h"

BindingEnvironment::BindingEnvironment(BindingEnvironment* parent) {
    this->parent = parent;
}

Binding* BindingEnvironment::get(string name) {
    map<string,Binding*>::iterator v = binding_map.find(name);
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
    Binding* b = new Binding();
    b->obj = o;
    b->type = Binding::SIMPLE;
    binding_map[name] = b;
}
