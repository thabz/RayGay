
#include "objects.h"
#include "scheme.h"
#include <sstream>

SchemeObject::SchemeObject(bool immutable) : immutable(immutable) {
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
// Symbol
//-----------------------------------------------------------
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

//-----------------------------------------------------------
// Char
//-----------------------------------------------------------
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
SchemePair::SchemePair() {
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

//-----------------------------------------------------------
// Vector
//-----------------------------------------------------------
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


//-----------------------------------------------------------
// Procedure
//-----------------------------------------------------------
SchemeProcedure::SchemeProcedure(SchemeObject* name, int req, int opt, int rst, SchemeObject* (*fn)()) {
    assert(s_symbol_p(name) == S_TRUE);
    this->name = static_cast<SchemeSymbol*>(name);
    assert(rst == 0 || rst == 1);
    this->req = req;
    this->opt = opt;
    this->rst = rst;
    this->fn = fn;
}

SchemeProcedure::SchemeProcedure(SchemeObject* name, BindingEnvironment* envt, SchemeObject* s_req, SchemeSymbol* s_rst, SchemeObject* s_body) {
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

//-----------------------------------------------------------
// Macro
//-----------------------------------------------------------
SchemeMacro::SchemeMacro(SchemeObject* name, BindingEnvironment* envt, SchemePair* s_req, SchemeSymbol* s_rst, SchemePair* s_body) : SchemeProcedure(name, envt, s_req, s_rst, s_body) {
}

string SchemeMacro::toString() {
    return "#<macro>";
}

//-----------------------------------------------------------
// Continuation
//-----------------------------------------------------------
SchemeContinuation::SchemeContinuation() {
}

void SchemeContinuation::call(SchemeObject* arg) {
    this->result = arg;
    longjmp(jmpbuf, 1);
}

string SchemeContinuation::toString() {
    return "#<continuation>";
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
