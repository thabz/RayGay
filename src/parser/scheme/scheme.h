
#ifndef SCHEME_SCHEME_H
#define SCHEME_SCHEME_H

#include "binding-env.h"

#include <iostream>
#include <string>
#include "objects.h"

using namespace std;

class Scheme {
    public:
        Scheme();
        SchemeObject* eval(string code);
        SchemeObject* eval(istream* code);

        // For assigning variables at top-level-frame
        void assign(string variable, double value);
        void assign(string variable, string value);
        void assign(string variable, bool value);
        void assign(string variable, SchemeObject* value);
        
        // For assigning built-in functions at top-level-frame
		void assign(string variable, int req, int opt, int rst, SchemeObject* (*fn)());
        
        // Look up in top-level-frame
        SchemeObject* lookup(string variable);

    private:
        BindingEnvironment* top_level_bindings;
};

class scheme_exception {
    public: 
		scheme_exception(string s) : str(s) {};
		string str;
};


// Scheme constants
extern SchemeBool* S_TRUE;
extern SchemeBool* S_FALSE;
extern SchemeUnspecified* S_UNSPECIFIED;
extern SchemeEmptyList* S_EMPTY_LIST;
extern SchemeNumber* S_ZERO;
extern SchemeNumber* S_ONE;
extern SchemeNumber* S_TWO;


// Scheme procedures
SchemeObject* s_display(BindingEnvironment* s, SchemeObject* o); 
SchemeObject* s_newline(BindingEnvironment* s);
SchemeObject* s_car(BindingEnvironment* s, SchemeObject* o);
SchemeObject* s_cdr(BindingEnvironment* s, SchemeObject* o);
SchemePair* s_cons(BindingEnvironment* s, SchemeObject* car, SchemeObject* cdr);
SchemeBool* s_boolean_p(BindingEnvironment* s, SchemeObject* o);
SchemeBool* s_procedure_p(BindingEnvironment* s, SchemeObject* o);
SchemeBool* s_list_p(BindingEnvironment* s, SchemeObject* p);
SchemePair* s_list(BindingEnvironment* s, SchemePair* args);
SchemeObject* s_list_ref(BindingEnvironment* s, SchemePair* l, SchemeNumber* index);
SchemePair* s_list_tail(BindingEnvironment* s, SchemePair* list, SchemeNumber* k);
SchemeBool* s_pair_p(BindingEnvironment* s, SchemeObject* p);
SchemeBool* s_symbol_p(BindingEnvironment* s, SchemeObject* p);
SchemePair* s_reverse(BindingEnvironment* s, SchemeObject* l);
SchemeNumber* s_length(BindingEnvironment* s, SchemePair* l);
SchemeNumber* s_plus(BindingEnvironment* s, SchemePair* l);
SchemeNumber* s_minus(BindingEnvironment* s, SchemePair* l);
SchemeNumber* s_mult(BindingEnvironment* s, SchemePair* l);
SchemeObject* s_list_ref(BindingEnvironment* s, SchemePair* l, SchemeNumber* i);
SchemeObject* s_apply(BindingEnvironment* s, SchemeProcedure* fn_name, SchemePair* args);
SchemeObject* s_map(BindingEnvironment* s, SchemeProcedure* fn_name, SchemePair* args);
SchemeBool* s_equal_p(BindingEnvironment* s, SchemeObject* a, SchemeObject* b);
SchemeObject* s_member(BindingEnvironment* s, SchemeObject* obj, SchemePair* p);

#endif
