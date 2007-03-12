
#ifndef SCHEME_SCHEME_H
#define SCHEME_SCHEME_H

#include <iostream>
#include <string>
#include "objects.h"

using namespace std;

class Scheme {
    public:
        Scheme();
        SchemeObject* eval(string code);
        SchemeObject* eval(istream* code);

        void assign(string variable, double value);
        void assign(string variable, string value);
        void assign(string variable, bool value);

        SchemeObject* lookup(string variable);

        // Scheme constants
        static SchemeBool* S_TRUE;
        static SchemeBool* S_FALSE;
        static SchemeUnspecified* S_UNSPECIFIED;
        static SchemeEmptyList* S_EMPTY_LIST;
		static SchemeNumber* S_ZERO;
		static SchemeNumber* S_ONE;

        void die_with_error(string error); 

        // Scheme procedures
        SchemeObject* display(SchemeObject* o); 
        SchemeObject* newline();
        SchemePair* cons(SchemeObject* car, SchemeObject* cdr);
        SchemeBool* boolean_p(SchemeObject* o);
        SchemeBool* list_p(SchemeObject* p);
        SchemeObject* reverse(SchemeObject* l);

    private:
        
};

#endif
