
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
SchemeObject* s_display(SchemeObject* o); 
SchemeObject* s_newline(BindingEnvironment* s);
SchemeObject* s_car(SchemeObject* o);
SchemeObject* s_cdr(SchemeObject* o);
SchemePair* s_cons(SchemeObject* car, SchemeObject* cdr);
SchemeBool* s_boolean_p(SchemeObject* o);
SchemeBool* s_string_p(SchemeObject* o);
SchemeBool* s_procedure_p(SchemeObject* o);
SchemeBool* s_list_p(SchemeObject* p);
SchemeBool* s_number_p(SchemeObject* p);
SchemeBool* s_vector_p(SchemeObject* p);
SchemePair* s_list(SchemePair* args);
SchemeObject* s_list_ref(SchemePair* l, SchemeNumber* index);
SchemePair* s_list_tail(SchemePair* list, SchemeNumber* k);
SchemeObject* s_append(SchemePair* args);
SchemeBool* s_pair_p(SchemeObject* p);
SchemeBool* s_null_p(SchemeObject* p);
SchemeBool* s_symbol_p(SchemeObject* p);
SchemePair* s_reverse(SchemeObject* l);
SchemeNumber* s_length(SchemePair* l);
SchemeObject* s_list_ref(SchemePair* l, SchemeNumber* i);
SchemeBool* s_equal_p(SchemeObject* a, SchemeObject* b);
SchemeObject* s_member(SchemeObject* obj, SchemePair* p);
SchemeVector* s_make_vector(SchemeNumber* count, SchemeObject* obj);
SchemeVector* s_vector(SchemePair* args);
SchemeNumber* s_vector_length(SchemeObject* v);
SchemeVector* s_list_2_vector(SchemePair* list);
SchemePair* s_vector_2_list(SchemeVector* v);
SchemeObject* s_vector_ref(SchemeVector* v, SchemeNumber* i);
SchemeObject* s_vector_set_e(SchemeVector* vec, SchemeNumber* index, SchemeObject* val);

// Math stuff
SchemeBool* s_equal(SchemePair* p);
SchemeBool* s_less(SchemePair* p);
SchemeBool* s_greater(SchemePair* p);
SchemeNumber* s_plus(SchemePair* l);
SchemeNumber* s_minus(SchemePair* l);
SchemeNumber* s_mult(SchemePair* l);
SchemeNumber* s_sqrt(SchemeNumber* n);
SchemeNumber* s_sin(SchemeNumber* n);
SchemeNumber* s_cos(SchemeNumber* n);
SchemeNumber* s_asin(SchemeNumber* n);
SchemeNumber* s_acos(SchemeNumber* n);
SchemeNumber* s_tan(SchemeNumber* n);
SchemeNumber* s_atan(SchemeNumber* y, SchemeObject* x);
SchemeNumber* s_expt(SchemeNumber* y, SchemeNumber* x);
SchemeNumber* s_exp(SchemeNumber* n);
SchemeNumber* s_log(SchemeNumber* n);
SchemeNumber* s_min(SchemeNumber* n, SchemePair* l);
SchemeNumber* s_max(SchemeNumber* n, SchemePair* l);

#endif
