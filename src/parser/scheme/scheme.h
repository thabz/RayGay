
#ifndef SCHEME_SCHEME_H
#define SCHEME_SCHEME_H

#include <iostream>
#include <sstream>
#include <string>
#include "objects.h"
#include "heap.h"

using namespace std;

class Scheme {
    public:
        Scheme();
        SchemeObject* eval(string code);
        SchemeObject* eval(istream* code);

        // For assigning variables at top-level-frame
        void assign(string variable, double value, SchemeEnvironment* b);
        void assign(string variable, string value, SchemeEnvironment* b);
        void assign(string variable, bool value, SchemeEnvironment* b);
        void assign(string variable, SchemeObject* value, SchemeEnvironment* b);
        
        // For assigning built-in functions at top-level-frame
		void assign(string variable, int req, int opt, int rst, SchemeObject* (*fn)(), SchemeEnvironment* b);
        
        // Look up in top-level-frame
        SchemeObject* lookup(string variable);

    private:
};

class scheme_exception {
    public: 
		scheme_exception(string s);
		string str;
};

// Faster internal macro for some much used procedures
// that does no argument checking.
#define i_car(o)         (static_cast<SchemePair*>(o)->car)
#define i_cdr(o)         (static_cast<SchemePair*>(o)->cdr)
#define i_caar(o)        (static_cast<SchemePair*>(static_cast<SchemePair*>(o)->car)->car)
#define i_cadr(o)        (static_cast<SchemePair*>(static_cast<SchemePair*>(o)->cdr)->car)
#define i_cdar(o)        (static_cast<SchemePair*>(static_cast<SchemePair*>(o)->car)->cdr)
#define i_cddr(o)        (static_cast<SchemePair*>(static_cast<SchemePair*>(o)->cdr)->cdr)
#define i_set_cdr_e(o,v) (static_cast<SchemePair*>(o)->cdr = (v))
#define i_pair_p(o)      ((o)->type() == SchemeObject::PAIR ? S_TRUE : S_FALSE)
#define i_symbol_p(o)    ((o)->type() == SchemeObject::SYMBOL ? S_TRUE : S_FALSE)
#define i_number_p(o)    ((o)->type() == SchemeObject::NUMBER ? S_TRUE : S_FALSE)
#define i_null_p(o)      ((o) == S_EMPTY_LIST ? S_TRUE : S_FALSE)
#define i_cons(car,cdr)  (SchemePair::create((car),(cdr)))

#define assert_arg_type(procname, argnum, test_fn, arg) { \
    if ((test_fn)(arg) == S_FALSE) {                      \
        ostringstream ss;                                 \
        ss << "Wrong argument-type in position ";         \
        ss << argnum;                                     \
        ss << " in call to ";                             \
        ss << string(procname);                           \
        ss << ": " << arg->toString();                    \
        throw scheme_exception(ss.str());                 \
    }                                                     \
}

#define assert_arg_pair_type(procname, argnum, arg) {     \
    if (i_pair_p(arg) == S_FALSE) {                       \
        ostringstream ss;                                 \
        ss << "Wrong argument-type in position ";         \
        ss << argnum;                                     \
        ss << " in call to ";                             \
        ss << string(procname);                           \
        ss << ": " << arg->toString();                    \
        throw scheme_exception(ss.str());                 \
    }                                                     \
}

#define assert_arg_number_type(procname, argnum, arg) {   \
    if (i_number_p(arg) == S_FALSE) {                     \
        ostringstream ss;                                 \
        ss << "Wrong argument-type in position ";         \
        ss << argnum;                                     \
        ss << " in call to ";                             \
        ss << string(procname);                           \
        ss << ": " << arg->toString();                    \
        throw scheme_exception(ss.str());                 \
    }                                                     \
}


void assert_arg_not_immutable(char* procname, int argnum, SchemeObject* arg);
void assert_arg_int_in_range(char* procname, int argnum, SchemeObject* arg, int from, int to);
void assert_arg_positive_int(char* procname, int argnum, SchemeObject* arg);

// Scheme constants
extern SchemeBool* S_TRUE;
extern SchemeBool* S_FALSE;
extern SchemeUnspecified* S_UNSPECIFIED;
extern SchemeEmptyList* S_EMPTY_LIST;
extern SchemeNumber* S_ZERO;
extern SchemeNumber* S_ONE;
extern SchemeNumber* S_TWO;

// Conversion macros
#define scm2int(o)     (int(static_cast<SchemeNumber*>(o)->number))
#define scm2double(o)  (static_cast<SchemeNumber*>(o)->number)
#define scm2string(o)  (static_cast<SchemeString*>(o)->str)
#define scm2char(o)    (char(static_cast<SchemeChar*>(o)->c))
#define scm2bool(o)    ((o) != S_FALSE)
#define bool2scm(b)    ((b) ? S_TRUE : S_FALSE)
#define string2scm(s)  (SchemeString::create(s))
#define char2scm(c)    (SchemeChar::create(c))
#define int2scm(n)     (((n) < 10 && (n) >= 0) ? S_NUMBERS[n] : SchemeNumber::create(n))
#define double2scm(n)  (SchemeNumber::create(n))

// Declaration of scheme procedures
SchemeBool* s_equal_p(SchemeObject* a, SchemeObject* b);
SchemeBool* s_eq_p(SchemeObject* a, SchemeObject* b);
SchemeBool* s_eqv_p(SchemeObject* a, SchemeObject* b);
SchemeBool* s_not(SchemeObject*);

SchemeObject* s_call_cc(SchemeObject* proc);

SchemeObject* s_apply(SchemeObject* proc, SchemeObject* args);
SchemeObject* s_null_environment(SchemeObject* version);
SchemeObject* s_scheme_report_environment(SchemeObject* version);
SchemeObject* s_interaction_environment(SchemeObject* version);
SchemeObject* s_eval(SchemeObject* expression, SchemeObject* environment);

SchemeObject* s_map(SchemeObject* proc, SchemeObject* lists);
SchemeObject* s_for_each(SchemeObject* proc, SchemeObject* lists);

SchemeBool* s_boolean_p(SchemeObject* o);
SchemeBool* s_string_p(SchemeObject* o);
SchemeBool* s_char_p(SchemeObject* o);
SchemeBool* s_procedure_p(SchemeObject* o);
SchemeBool* s_list_p(SchemeObject* p);
SchemeBool* s_circular_list_p(SchemeObject* p);
SchemeBool* s_number_p(SchemeObject* p);
SchemeBool* s_vector_p(SchemeObject* p);
SchemeBool* s_pair_p(SchemeObject* p);
SchemeBool* s_null_p(SchemeObject* p);
SchemeBool* s_symbol_p(SchemeObject* p);

SchemeObject* s_assoc(SchemeObject* obj, SchemePair* alist);
SchemeObject* s_assq(SchemeObject* obj, SchemePair* alist);
SchemeObject* s_assv(SchemeObject* obj, SchemePair* alist);

SchemeObject* s_car(SchemeObject* o);
SchemeObject* s_cdr(SchemeObject* o);
SchemeObject* s_cadr(SchemeObject* o);
SchemeObject* s_cdar(SchemeObject* o);
SchemeObject* s_cddr(SchemeObject* o);
SchemeObject* s_caaar(SchemeObject* o);
SchemeObject* s_caadr(SchemeObject* o);
SchemeObject* s_cadar(SchemeObject* o);
SchemeObject* s_caddr(SchemeObject* o);
SchemeObject* s_cdaar(SchemeObject* o);
SchemeObject* s_cdadr(SchemeObject* o);
SchemeObject* s_cddar(SchemeObject* o);
SchemeObject* s_cdddr(SchemeObject* o);

SchemePair* s_cons(SchemeObject* car, SchemeObject* cdr);
SchemeObject* s_set_car_e(SchemeObject* p, SchemeObject* o);
SchemeObject* s_set_cdr_e(SchemeObject* p, SchemeObject* o);
SchemeObject* s_append(SchemeObject* args);
SchemeObject* s_reverse(SchemeObject* l);
SchemeNumber* s_length(SchemeObject* l);
SchemeObject* s_list(SchemeObject* args);
SchemeObject* s_list_tail(SchemeObject* l, SchemeObject* k);
SchemeObject* s_list_ref(SchemeObject* l, SchemeObject* k);
SchemeObject* s_member(SchemeObject* obj, SchemeObject* p);
SchemeObject* s_memq(SchemeObject* obj, SchemeObject* p);
SchemeObject* s_memv(SchemeObject* obj, SchemeObject* p);

// Vector stuff
SchemeVector* s_make_vector(SchemeObject* count, SchemeObject* obj);
SchemeVector* s_vector(SchemeObject* args);
SchemeNumber* s_vector_length(SchemeObject* v);
SchemeVector* s_list_2_vector(SchemeObject* list);
SchemePair* s_vector_2_list(SchemeObject* v);
SchemeObject* s_vector_ref(SchemeObject* v, SchemeObject* i);
SchemeObject* s_vector_set_e(SchemeObject* vec, SchemeObject* index, SchemeObject* val);
SchemeObject* s_vector_fill_e(SchemeObject* vec, SchemeObject* fill);

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
SchemeBool* s_equal(SchemeObject* p);
SchemeBool* s_less(SchemeObject* p);
SchemeBool* s_greater(SchemeObject* p);
SchemeBool* s_less_equal(SchemeObject* p);
SchemeBool* s_greater_equal(SchemeObject* p);
SchemeNumber* s_plus(SchemeObject* l);
SchemeNumber* s_minus(SchemeObject* n, SchemeObject* rst);
SchemeNumber* s_mult(SchemeObject* l);
SchemeNumber* s_divide(SchemeObject* n, SchemeObject* rst);
SchemeNumber* s_sqrt(SchemeObject* n);
SchemeNumber* s_abs(SchemeObject* n);
SchemeNumber* s_sin(SchemeObject* n);
SchemeNumber* s_cos(SchemeObject* n);
SchemeNumber* s_asin(SchemeObject* n);
SchemeNumber* s_acos(SchemeObject* n);
SchemeNumber* s_tan(SchemeObject* n);
SchemeNumber* s_atan(SchemeObject* y, SchemeObject* x);
SchemeNumber* s_expt(SchemeObject* y, SchemeObject* x);
SchemeNumber* s_exp(SchemeObject* n);
SchemeNumber* s_log(SchemeObject* n);
SchemeObject* s_min(SchemeObject* n, SchemeObject* l);
SchemeObject* s_max(SchemeObject* n, SchemeObject* l);
SchemeObject* s_gcd(SchemeObject* l);
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
SchemeChar* s_string_ref(SchemeObject* s, SchemeNumber* i);
SchemeObject* s_string_set_e(SchemeObject* str, SchemeObject* i, SchemeObject* chr);
SchemeString* s_symbol_2_string(SchemeObject* symbol);
SchemeSymbol* s_string_2_symbol(SchemeObject* s);
SchemeObject* s_number_2_string(SchemeObject* n, SchemeObject* base);
SchemeObject* s_string_2_number(SchemeObject* s, SchemeObject* base);
SchemeString* s_string_append(SchemeObject* strings);
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
SchemeObject* s_write_char(SchemeObject* s_char, SchemeObject* s_port);
SchemeObject* s_read(SchemeObject* s_port);
SchemeObject* s_write(SchemeObject* o, SchemeObject* port);
SchemeObject* s_display(SchemeObject* o, SchemeObject* port); 
SchemeObject* s_newline(SchemeObject* port );

extern SchemeSymbol* if_symbol;
extern SchemeSymbol* cond_symbol;
extern SchemeSymbol* apply_symbol;
extern SchemeSymbol* else_symbol;
extern SchemeSymbol* ergo_symbol;
extern SchemeSymbol* case_symbol;
extern SchemeSymbol* do_symbol;
extern SchemeSymbol* let_symbol;
extern SchemeSymbol* letstar_symbol;
extern SchemeSymbol* letrec_symbol;
extern SchemeSymbol* begin_symbol;
extern SchemeSymbol* and_symbol;
extern SchemeSymbol* or_symbol;
extern SchemeSymbol* lambda_symbol;
extern SchemeSymbol* quote_symbol;
extern SchemeSymbol* quasiquote_symbol;
extern SchemeSymbol* unquote_symbol;
extern SchemeSymbol* unquote_splicing_symbol;
extern SchemeSymbol* define_symbol;
extern SchemeSymbol* define_macro;
extern SchemeSymbol* set_e_symbol;
extern SchemeSymbol* unnamed_symbol;

#endif
