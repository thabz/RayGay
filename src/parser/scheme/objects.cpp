
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
SchemeSymbol::SchemeSymbol(string s) : str(s) { 
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
	return static_cast<SchemePair*>(cdr);
}

string SchemePair::toString() {
	string result = "(";
	SchemePair* p = this;
    while (true) {
		result += p->car->toString();
		SchemeObject* n = p->cdr;
		if (n == Scheme::S_EMPTY_LIST) {
			break;
		}
		SchemePair* l = static_cast<SchemePair*> (n);
		if (l == NULL) {
			result += " . " + p->cdr->toString();
			break;
		}
		p = l;
		result += " ";
	}
	result += ")";
	return result;
}
