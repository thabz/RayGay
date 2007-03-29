
#include "binding-env.h"
#include "scheme.h"

BindingEnvironment::BindingEnvironment(BindingEnvironment* parent) {
    this->parent = parent;
}

SchemeObject* BindingEnvironment::get(SchemeSymbol* name) {
    map<SchemeSymbol*,SchemeObject*>::iterator v = binding_map.find(name);
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

void BindingEnvironment::put(SchemeSymbol* name, SchemeObject* o) {
    binding_map[name] = o;
}

void BindingEnvironment::set(SchemeSymbol* name, SchemeObject* o) {
    map<SchemeSymbol*,SchemeObject*>::iterator v = binding_map.find(name);
    if (v == binding_map.end()) {
        if (parent != NULL) {
            parent->set(name,o);
        } else {
            throw scheme_exception("Unbound variable: " + name->toString());
        }
    } else {
        put(name,o);
    }
}
