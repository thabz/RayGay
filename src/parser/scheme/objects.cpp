
#include "objects.h"
#include "scheme.h"
#include <sstream>
#include "heap.h"

SchemeObject::SchemeObject(bool immutable) : immutable(immutable) {
    in_use = false;
    Heap::getUniqueInstance()->addAllocation(this);
}

void SchemeObject::mark() {
    in_use = true;
}

//-----------------------------------------------------------
// Unspecified
//-----------------------------------------------------------
string SchemeUnspecified::toString() {
    return "#<unspecified>";
}

//-----------------------------------------------------------
// String
//-----------------------------------------------------------
SchemeString* SchemeString::create(string s, bool immutable) {
    return new SchemeString(s, immutable);
}

SchemeString::SchemeString(string s, bool immutable) : SchemeObject(immutable), str(s) {
    
}

string SchemeString::toString() {
    // TODO: s!"!\"!g and s!\!\\!g
    return "\"" + str + "\"";
}

//-----------------------------------------------------------
// Symbol
//-----------------------------------------------------------
map<string,SchemeSymbol*> SchemeSymbol::known_symbols;

SchemeSymbol::SchemeSymbol(string s) : str(s) { 
}

SchemeSymbol* SchemeSymbol::create(string s) {
    SchemeSymbol* result;
    map<string,SchemeSymbol*>::iterator v = known_symbols.find(s);
    if (v == known_symbols.end()) {
        result = new SchemeSymbol(s);
        known_symbols[s] = result;
    } else {
        result = v->second;
    }
    return result;
}

string SchemeSymbol::toString() {
    return str;
}

//-----------------------------------------------------------
// Number
//-----------------------------------------------------------
SchemeNumber* SchemeNumber::create(double s) {
    return new SchemeNumber(s);
}

SchemeNumber::SchemeNumber(double s) : number(s) { 
}

string SchemeNumber::toString() {
    ostringstream ss;
    ss << number;
    return ss.str();
}

//-----------------------------------------------------------
// Boolean
//-----------------------------------------------------------
SchemeBool::SchemeBool(bool b) : boolean(b) { 
}

string SchemeBool::toString() {
    return boolean ? "#t" : "#f";
}

//-----------------------------------------------------------
// Empty list
//-----------------------------------------------------------
string SchemeEmptyList::toString() {
    return "()";
}

void SchemeEmptyList::mark() {
    SchemeObject::mark();
}

//-----------------------------------------------------------
// Char
//-----------------------------------------------------------
SchemeChar* SchemeChar::create(char c) {
    return new SchemeChar(c);
}

SchemeChar::SchemeChar(char c) : c(c) { 
}

string SchemeChar::toString() {
    if (c == ' ') {
        return string("#\\space");
    } else if (c == '\n') {
        return string("#\\newline");
    } else {
        return string("#\\")+string(&c,1);
    }
}

//-----------------------------------------------------------
// Pair
//-----------------------------------------------------------
SchemePair* SchemePair::create() {
    return new SchemePair();
}

SchemePair* SchemePair::create(SchemeObject* car, SchemeObject* cdr) {
    return new SchemePair(car,cdr);
}

SchemePair::SchemePair() {
    this->car = NULL;
    this->cdr = NULL;
}

SchemePair::SchemePair(SchemeObject* car, SchemeObject* cdr) {
    this->car = car;
    this->cdr = cdr;
}

SchemePair* SchemePair::cdrAsPair() {
    if (cdr == S_EMPTY_LIST || cdr->type() == SchemeObject::PAIR) {
	    return static_cast<SchemePair*>(cdr);
    } else {
        return NULL;
    }  
}

string SchemePair::toString() {
    if (s_circular_list_p(this) == S_TRUE) {
        return "#<circular list>";
    }
	string result = "(";
	SchemePair *p = this, *n;
    while (true) {
		result += p->car->toString();
		n = p->cdrAsPair();
		if (n == S_EMPTY_LIST) {
			break;
		}
		if (n == NULL) {
			result += " . " + p->cdr->toString();
			break;
		}
        p = n;
		result += " ";
	}
	result += ")";
	return result;
}

void SchemePair::mark() {
    if (in_use == false) {
        SchemeObject::mark();
        if (car != NULL) {
            car->mark();
        }
        if (cdr != NULL) {
            cdr->mark();
        }
    }
}

//-----------------------------------------------------------
// Vector
//-----------------------------------------------------------
SchemeVector* SchemeVector::create(SchemeObject** elems, int length) {
    return new SchemeVector(elems,length);
}

SchemeVector* SchemeVector::create(SchemeObject* elem, int length) {
    return new SchemeVector(elem,length);
}

SchemeVector::SchemeVector(SchemeObject** elems, int length) {
    this->elems = elems;
    this->length = length;
}

SchemeVector::SchemeVector(SchemeObject* elem, int length) {
    this->elems = new SchemeObject*[length];
    this->length = length;
    for(int i = 0; i < length; i++) {
        elems[i] = elem;
    }
}


SchemeObject* SchemeVector::get(int i) {
    return elems[i];
}

void SchemeVector::set(SchemeObject* o, int i) {
    elems[i] = o;
}

string SchemeVector::toString() {
    string result = "#(";
    for(int i = 0; i < length; i++) {
        result += elems[i]->toString();
        if (i < length-1) {
            result += " ";
        }
    }
    result += ")";
    return result;
}

void SchemeVector::mark() {
    if (in_use == false) {
        SchemeObject::mark();
        for(int i = 0; i < length; i++) {
            elems[i]->mark();
        }    
    }
}

//-----------------------------------------------------------
// Procedure
//-----------------------------------------------------------

SchemeProcedure* SchemeProcedure::create(SchemeObject* name, int req, int opt, int rst, SchemeObject* (*fn)()) {
    return new SchemeProcedure(name,req,opt,rst,fn);
}

SchemeProcedure* SchemeProcedure::create(SchemeObject* name, SchemeEnvironment* envt, SchemeObject* s_req, SchemeSymbol* s_rst, SchemeObject* s_body) {
    return new SchemeProcedure(name,envt,s_req, s_rst, s_body);
}


SchemeProcedure::SchemeProcedure(SchemeObject* name, int req, int opt, int rst, SchemeObject* (*fn)()) {
    assert(s_symbol_p(name) == S_TRUE);
    this->name = static_cast<SchemeSymbol*>(name);
    assert(rst == 0 || rst == 1);
    this->req = req;
    this->opt = opt;
    this->rst = rst;
    this->fn = fn;
}

SchemeProcedure::SchemeProcedure(SchemeObject* name, SchemeEnvironment* envt, SchemeObject* s_req, SchemeSymbol* s_rst, SchemeObject* s_body) {
    assert(s_symbol_p(name) == S_TRUE);
    this->name = static_cast<SchemeSymbol*>(name);
    this->envt = envt;
    this->fn = NULL;
    this->s_rst = s_rst;
    this->s_req = s_req;
    this->s_body = s_body;
    rst = (s_rst == NULL) ? 0 : 1;
    req = int(s_length(s_req)->number);
}

string SchemeProcedure::toString() {
    return "#<primitive-procedure "+name->str+">";
}

void SchemeProcedure::setName(SchemeObject* name) {
    assert(s_symbol_p(name) == S_TRUE);
    this->name = static_cast<SchemeSymbol*>(name);
}

void SchemeProcedure::mark() {
    if (in_use == false) {
        SchemeObject::mark();
    
        if (name != NULL) {
            name->mark();
        }
        if (fn == NULL) {
            envt->mark();
            s_req->mark();
            if (s_rst != NULL) s_rst->mark();
            s_body->mark(); 
        }
    }
}

//-----------------------------------------------------------
// Internal procedure
//-----------------------------------------------------------
string SchemeInternalProcedure::toString() {
    return "#<internal-procedure "+name+">";
}


//-----------------------------------------------------------
// Macro
//-----------------------------------------------------------
SchemeMacro* SchemeMacro::create(SchemeObject* name, SchemeEnvironment* envt, SchemePair* s_req, SchemeSymbol* s_rst, SchemePair* s_body) {
    return new SchemeMacro(name,envt,s_req,s_rst,s_body);
    
}
SchemeMacro::SchemeMacro(SchemeObject* name, SchemeEnvironment* envt, SchemePair* s_req, SchemeSymbol* s_rst, SchemePair* s_body) : SchemeProcedure(name, envt, s_req, s_rst, s_body) {
}

string SchemeMacro::toString() {
    return "#<macro>";
}

//-----------------------------------------------------------
// Continuation
//-----------------------------------------------------------
SchemeContinuation::SchemeContinuation() {
    this->result = NULL;
}

void SchemeContinuation::call(SchemeObject* arg) {
    this->result = arg;
    longjmp(jmpbuf, 1);
}

string SchemeContinuation::toString() {
    return "#<continuation>";
}

void SchemeContinuation::mark() {
    if (result != NULL) {
        result->mark();
    }
    SchemeObject::mark();
}

//-----------------------------------------------------------
// Input- and output ports
//-----------------------------------------------------------
SchemeInputPort::SchemeInputPort(istream* i) {
    this->is = i;
}

SchemeOutputPort::SchemeOutputPort(ostream* o) {
    this->os = o;
}

string SchemeInputPort::toString() {
    return "#<input-port>";
}

string SchemeOutputPort::toString() {
    return "#<output-port>";
}

SchemeEOF::SchemeEOF() { 
}

string SchemeEOF::toString() { 
    return "#<EOF>";
}

//-----------------------------------------------------------
// Environment
//-----------------------------------------------------------

SchemeEnvironment* SchemeEnvironment::create(SchemeEnvironment* parent) {
    return new SchemeEnvironment(parent);
}

string SchemeEnvironment::toString() { 
    return "#<environment>";
}

SchemeEnvironment::SchemeEnvironment(SchemeEnvironment* parent) {
    this->parent = parent;
}

SchemeObject* SchemeEnvironment::get(SchemeObject* name) {
    if (s_symbol_p(name) == S_FALSE) {
        cout << name->toString() << " is not a symbol." << endl;
    }
    map<SchemeObject*,SchemeObject*>::iterator v = binding_map.find(name);
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

void SchemeEnvironment::put(SchemeObject* name, SchemeObject* o) {
    if (s_symbol_p(name) == S_FALSE) {
        cout << name->toString() << " is not a symbol." << endl;
    }
    binding_map[name] = o;
}

void SchemeEnvironment::set(SchemeObject* name, SchemeObject* o) {
    if (s_symbol_p(name) == S_FALSE) {
        cout << name->toString() << " is not a symbol." << endl;
    }
    map<SchemeObject*,SchemeObject*>::iterator v = binding_map.find(name);
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

void SchemeEnvironment::mark() {
    if (in_use == false) {
        SchemeObject::mark();
        map<SchemeObject*,SchemeObject*>::iterator v = binding_map.begin();
        while (v != binding_map.end()) {
            if ((*v).first != NULL) (*v).first->mark();
            if ((*v).second != NULL) (*v).second->mark();
            v++;
        }
        if (parent != NULL) {
            parent->mark();
        }
    }
}



