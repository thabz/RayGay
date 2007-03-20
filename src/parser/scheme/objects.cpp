
#include "objects.h"
#include "scheme.h"
#include <sstream>


//-----------------------------------------------------------
// Unspecified
//-----------------------------------------------------------
string SchemeUnspecified::toString() {
    return "#<unspecified>";
}

//-----------------------------------------------------------
// String
//-----------------------------------------------------------
SchemeString::SchemeString(string s) : str(s) {
    
}

string SchemeString::toString() {
    return "\"" + str + "\"";
}

//-----------------------------------------------------------
// Symbol
//-----------------------------------------------------------
map<string,SchemeSymbol*> SchemeSymbol::symbols;

SchemeSymbol::SchemeSymbol(string s) : str(s) { 
}

SchemeSymbol* SchemeSymbol::create(string s) {
    SchemeSymbol* result;
    map<string,SchemeSymbol*>::iterator v = symbols.find(s);
    if (v == symbols.end()) {
        result = new SchemeSymbol(s);
        symbols[s] = result;
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

string SchemeEmptyList::toString() {
    return "()";
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
// Procedure
//-----------------------------------------------------------
SchemeProcedure::SchemeProcedure(int req, int opt, int rst, SchemeObject* (*fn)()) {
    assert(rst == 0 || rst == 1);
    this->req = req;
    this->opt = opt;
    this->rst = rst;
    this->fn = fn;
}

SchemeProcedure::SchemeProcedure(BindingEnvironment* envt, SchemePair* s_req, SchemeSymbol* s_rst, SchemePair* s_body) {
    this->envt = envt;
    this->s_rst = s_rst;
    this->s_req = s_req;
    this->s_body = s_body;
    rst = (s_rst == NULL) ? 0 : 1;
    req = int(s_length(s_req)->number);
}

string SchemeProcedure::toString() {
    return "#<primitive-procedure>";
}
