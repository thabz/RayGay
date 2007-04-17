
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
		scheme_exception(string s);
		string str;
};

void assert_arg_type(char* procname, int argnum, SchemeBool* (*)(SchemeObject*), SchemeObject* arg);

// Scheme constants
extern SchemeBool* S_TRUE;
extern SchemeBool* S_FALSE;
extern SchemeUnspecified* S_UNSPECIFIED;
extern SchemeEmptyList* S_EMPTY_LIST;
extern SchemeNumber* S_ZERO;
extern SchemeNumber* S_ONE;
extern SchemeNumber* S_TWO;

// Conversion 
#define scm2int(o)     (int(static_cast<SchemeNumber*>(o)->number))
#define scm2string(o)  (static_cast<SchemeString*>(o)->str)
#define scm2char(o)    (char(static_cast<SchemeChar*>(o)->c))
#define scm2bool(o)    ((o) != S_FALSE)
#define bool2scm(b)    ((b) ? S_TRUE : S_FALSE)
#define string2scm(s)  (new SchemeString(s))
#define char2scm(c)    (new SchemeChar(c))

// Scheme procedures
SchemeBool* s_equal_p(SchemeObject* a, SchemeObject* b);
SchemeBool* s_eq_p(SchemeObject* a, SchemeObject* b);
SchemeBool* s_eqv_p(SchemeObject* a, SchemeObject* b);
SchemeBool* s_not(SchemeObject*);

SchemeObject* s_write(SchemeObject* o, SchemeObject* port);
SchemeObject* s_display(SchemeObject* o, SchemeObject* port); 
SchemeObject* s_newline(SchemeObject* port );

SchemeObject* s_apply(SchemeObject* proc, SchemeObject* args);
SchemeObject* s_map(SchemeObject* proc, SchemeObject* lists);
SchemeObject* s_for_each(SchemeObject* proc, SchemeObject* lists);

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
SchemeNumber* s_length(SchemeObject* l);
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

// Char stuff
SchemeChar* s_char_upcase(SchemeObject* c);
SchemeChar* s_char_downcase(SchemeObject* c);
SchemeBool* s_char_alphabetic_p(SchemeObject* c);
SchemeBool* s_char_numeric_p(SchemeObject* c);
SchemeBool* s_char_whitespace_p(SchemeObject* c);
SchemeBool* s_char_upper_case_p(SchemeObject* c);
SchemeBool* s_char_lower_case_p(SchemeObject* c);
SchemeChar* s_integer_2_char(SchemeObject* i);
SchemeNumber* s_char_2_integer(SchemeObject* c);

// Math stuff
SchemeBool* s_equal(SchemePair* p);
SchemeBool* s_less(SchemePair* p);
SchemeBool* s_greater(SchemePair* p);
SchemeBool* s_less_equal(SchemePair* p);
SchemeBool* s_greater_equal(SchemePair* p);
SchemeNumber* s_plus(SchemePair* l);
SchemeNumber* s_minus(SchemePair* l);
SchemeNumber* s_mult(SchemeObject* l);
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
SchemeNumber* s_gcd(SchemeObject* l);
SchemeNumber* s_lcm(SchemeObject* l);
SchemeNumber* s_round(SchemeObject* n);
SchemeNumber* s_floor(SchemeObject* n);
SchemeNumber* s_ceiling(SchemeObject* n);
SchemeNumber* s_truncate(SchemeObject* n);
SchemeBool* s_even_p(SchemeObject* n);
SchemeBool* s_odd_p(SchemeObject* n);
SchemeBool* s_zero_p(SchemeObject* n);
SchemeBool* s_negative_p(SchemeObject* n);
SchemeBool* s_positive_p(SchemeObject* n);
SchemeBool* s_integer_p(SchemeObject* n);
SchemeBool* s_complex_p(SchemeObject* n);
SchemeBool* s_rational_p(SchemeObject* n);
SchemeBool* s_real_p(SchemeObject* n);
SchemeBool* s_exact_p(SchemeObject* n);
SchemeBool* s_inexact_p(SchemeObject* n);
SchemeNumber* s_quotient(SchemeObject* n1, SchemeObject* n2);
SchemeNumber* s_remainder(SchemeObject* n1, SchemeObject* n2);
SchemeNumber* s_modulo(SchemeObject* n1, SchemeObject* n2);

// String stuff
SchemeString* s_make_string(SchemeObject* len, SchemeObject* chr);
SchemeString* s_string(SchemeObject* chars);
SchemeNumber* s_string_length(SchemeObject* s);
SchemeChar* s_string_ref(SchemeString* s, SchemeNumber* i);
SchemeObject* s_string_set_e(SchemeObject* str, SchemeObject* i, SchemeObject* chr);
SchemeString* s_symbol_2_string(SchemeSymbol* symbol);
SchemeSymbol* s_string_2_symbol(SchemeString* s);
SchemeObject* s_number_2_string(SchemeObject* n, SchemeObject* base);
SchemeObject* s_string_2_number(SchemeObject* s, SchemeObject* base);
SchemeString* s_string_append(SchemePair* strings);
SchemeString* s_string_copy(SchemeObject* string);
SchemePair* s_string_2_list(SchemeObject* s);
SchemeString* s_list_2_string(SchemeObject* p);
SchemeString* s_substring(SchemeObject* str, SchemeObject* start, SchemeObject* end);
SchemeSymbol* s_symgen();


// Input and output stuff
SchemeInputPort* s_current_input_port();
SchemeOutputPort* s_current_output_port();
SchemeBool* s_input_port_p(SchemeObject* o);
SchemeBool* s_output_port_p(SchemeObject* o);
SchemeBool* s_eof_object_p(SchemeObject* o);
SchemeInputPort* s_open_input_file(SchemeObject* s_filename);
SchemeOutputPort* s_open_output_file(SchemeObject* s_filename);
SchemeObject* s_close_input_port(SchemeObject* s_port);
SchemeObject* s_close_output_port(SchemeObject* s_port);
SchemeObject* s_call_with_input_file(SchemeObject* s_filename, SchemeObject* proc);
SchemeObject* s_call_with_output_file(SchemeObject* s_filename, SchemeObject* proc);
SchemeObject* s_with_input_from_file(SchemeObject* s_filename, SchemeObject* thunk);
SchemeObject* s_with_output_to_file(SchemeObject* s_filename, SchemeObject* thunk);
SchemeObject* s_read_char(SchemeObject* s_port);
SchemeObject* s_peek_char(SchemeObject* s_port);

#endif
