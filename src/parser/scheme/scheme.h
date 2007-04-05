
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
SchemeBool* s_equal_p(SchemeObject* a, SchemeObject* b);
SchemeBool* s_eq_p(SchemeObject* a, SchemeObject* b);
SchemeBool* s_eqv_p(SchemeObject* a, SchemeObject* b);
SchemeBool* s_not(SchemeObject*);

SchemeObject* s_write(SchemeObject* o);
SchemeObject* s_display(SchemeObject* o); 
SchemeObject* s_newline(BindingEnvironment* s);

SchemeBool* s_boolean_p(SchemeObject* o);
SchemeBool* s_string_p(SchemeObject* o);
SchemeBool* s_char_p(SchemeObject* o);
SchemeBool* s_procedure_p(SchemeObject* o);
SchemeBool* s_list_p(SchemeObject* p);
SchemeBool* s_number_p(SchemeObject* p);
SchemeBool* s_vector_p(SchemeObject* p);
SchemeBool* s_pair_p(SchemeObject* p);
SchemeBool* s_null_p(SchemeObject* p);
SchemeBool* s_symbol_p(SchemeObject* p);
SchemePair* s_list(SchemePair* args);
SchemeObject* s_list_ref(SchemePair* l, SchemeNumber* index);
SchemePair* s_list_tail(SchemePair* list, SchemeNumber* k);
SchemeObject* s_assoc(SchemeObject* obj, SchemePair* alist);
SchemeObject* s_assq(SchemeObject* obj, SchemePair* alist);
SchemeObject* s_assv(SchemeObject* obj, SchemePair* alist);

SchemeObject* s_car(SchemeObject* o);
SchemeObject* s_cdr(SchemeObject* o);
SchemeObject* s_cadr(SchemeObject* o);
SchemeObject* s_cdar(SchemeObject* o);

SchemePair* s_cons(SchemeObject* car, SchemeObject* cdr);
SchemeObject* s_set_car_e(SchemeObject* p, SchemeObject* o);
SchemeObject* s_set_cdr_e(SchemeObject* p, SchemeObject* o);
SchemeObject* s_append(SchemePair* args);
SchemePair* s_reverse(SchemeObject* l);
SchemeNumber* s_length(SchemePair* l);
SchemeObject* s_list_ref(SchemePair* l, SchemeNumber* i);
SchemeObject* s_member(SchemeObject* obj, SchemeObject* p);
SchemeObject* s_memq(SchemeObject* obj, SchemeObject* p);
SchemeObject* s_memv(SchemeObject* obj, SchemeObject* p);
SchemeVector* s_make_vector(SchemeNumber* count, SchemeObject* obj);
SchemeVector* s_vector(SchemePair* args);
SchemeNumber* s_vector_length(SchemeObject* v);
SchemeVector* s_list_2_vector(SchemeObject* list);
SchemePair* s_vector_2_list(SchemeObject* v);
SchemeObject* s_vector_ref(SchemeVector* v, SchemeNumber* i);
SchemeObject* s_vector_set_e(SchemeVector* vec, SchemeNumber* index, SchemeObject* val);
SchemeObject* s_vector_fill_e(SchemeVector* vec, SchemeObject* fill);

// Math stuff
SchemeBool* s_equal(SchemePair* p);
SchemeBool* s_less(SchemePair* p);
SchemeBool* s_greater(SchemePair* p);
SchemeBool* s_less_equal(SchemePair* p);
SchemeBool* s_greater_equal(SchemePair* p);
SchemeNumber* s_plus(SchemePair* l);
SchemeNumber* s_minus(SchemePair* l);
SchemeNumber* s_mult(SchemePair* l);
SchemeNumber* s_divide(SchemePair* l);
SchemeNumber* s_sqrt(SchemeNumber* n);
SchemeNumber* s_abs(SchemeNumber* n);
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
SchemeBool* s_even_p(SchemeNumber* n);
SchemeBool* s_odd_p(SchemeNumber* n);
SchemeBool* s_zero_p(SchemeNumber* n);
SchemeBool* s_negative_p(SchemeNumber* n);
SchemeBool* s_positive_p(SchemeNumber* n);

// String stuff
SchemeString* s_make_string(SchemeObject* len, SchemeObject* chr);
SchemeNumber* s_string_length(SchemeObject* s);
SchemeChar* s_string_ref(SchemeString* s, SchemeNumber* i);
SchemeString* s_symbol_2_string(SchemeSymbol* symbol);
SchemeSymbol* s_string_2_symbol(SchemeString* s);
SchemeString* s_string_append(SchemePair* strings);
SchemeString* s_string_copy(SchemeObject* string);
SchemeSymbol* s_symgen();

#endif
