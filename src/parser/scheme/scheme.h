
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
        void assign(string variable, double value, SchemeObject* b);
        void assign(string variable, string value, SchemeObject* b);
        void assign(string variable, bool value, SchemeObject* b);
        void assign(string variable, SchemeObject* value, SchemeObject* b);
        
        // For assigning built-in functions at top-level-frame
		void assign(string variable, int req, int opt, int rst, SchemeObject* (*fn)(), SchemeObject* b);
        
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
#define i_car(o)         ((o)->car)
#define i_cdr(o)         ((o)->cdr)
#define i_caar(o)        (static_cast<SchemeObject*>(static_cast<SchemeObject*>(o)->car)->car)
#define i_cadr(o)        (static_cast<SchemeObject*>(static_cast<SchemeObject*>(o)->cdr)->car)
#define i_cdar(o)        (static_cast<SchemeObject*>(static_cast<SchemeObject*>(o)->car)->cdr)
#define i_cddr(o)        (static_cast<SchemeObject*>(static_cast<SchemeObject*>(o)->cdr)->cdr)
#define i_set_cdr_e(o,v) (static_cast<SchemeObject*>(o)->cdr = (v))
#define i_pair_p(o)      ((o)->type() == SchemeObject::PAIR ? S_TRUE : S_FALSE)
#define i_char_p(o)      ((o)->type() == SchemeObject::CHAR ? S_TRUE : S_FALSE)
#define i_symbol_p(o)    ((o)->type() == SchemeObject::SYMBOL ? S_TRUE : S_FALSE)
#define i_number_p(o)    ((o)->type() == SchemeObject::NUMBER ? S_TRUE : S_FALSE)
#define i_procedure_p(p) (((p)->type() == SchemeObject::BUILT_IN_PROCEDURE ||  \
                           (p)->type() == SchemeObject::CONTINUATION       ||  \
                           (p)->type() == SchemeObject::USER_PROCEDURE     ||  \
                           (p)->type() == SchemeObject::INTERNAL_PROCEDURE) ? S_TRUE : S_FALSE)
#define i_null_p(o)      ((o) == S_EMPTY_LIST ? S_TRUE : S_FALSE)
#define i_cons(car,cdr)  (SchemeObject::createPair((car),(cdr)))

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
extern SchemeObject* S_TRUE;
extern SchemeObject* S_FALSE;
extern SchemeObject* S_UNSPECIFIED;
extern SchemeObject* S_EMPTY_LIST;
extern SchemeObject* S_ZERO;
extern SchemeObject* S_ONE;
extern SchemeObject* S_TWO;

// Conversion macros
#define scm2int(o)     (int((o)->value))
#define scm2double(o)  ((o)->value)
#define scm2string(o)  (string((o)->str))
#define scm2char(o)    ((o)->c)
#define scm2bool(o)    ((o) != S_FALSE)
#define bool2scm(b)    ((b) ? S_TRUE : S_FALSE)
#define string2scm(s)  (SchemeObject::createString(s.c_str()))
#define cstr2scm(s)    (SchemeObject::createString(s))
#define char2scm(c)    (SchemeObject::createChar(c))
#define int2scm(n)     (((n) < 10 && (n) >= 0) ? S_NUMBERS[n] : SchemeObject::createNumber(n))
#define double2scm(n)  (SchemeObject::createNumber(n))

// Declaration of scheme procedures
SchemeObject* s_equal_p(SchemeObject* a, SchemeObject* b);
SchemeObject* s_eq_p(SchemeObject* a, SchemeObject* b);
SchemeObject* s_eqv_p(SchemeObject* a, SchemeObject* b);
SchemeObject* s_not(SchemeObject*);

SchemeObject* s_call_cc(SchemeObject* proc);

SchemeObject* s_apply(SchemeObject* proc, SchemeObject* args);
SchemeObject* s_null_environment(SchemeObject* version);
SchemeObject* s_scheme_report_environment(SchemeObject* version);
SchemeObject* s_interaction_environment(SchemeObject* version);
SchemeObject* s_eval(SchemeObject* expression, SchemeObject* environment);

SchemeObject* s_map(SchemeObject* proc, SchemeObject* lists);
SchemeObject* s_for_each(SchemeObject* proc, SchemeObject* lists);

SchemeObject* s_boolean_p(SchemeObject* o);
SchemeObject* s_string_p(SchemeObject* o);
SchemeObject* s_char_p(SchemeObject* o);
SchemeObject* s_procedure_p(SchemeObject* o);
SchemeObject* s_list_p(SchemeObject* p);
SchemeObject* s_circular_list_p(SchemeObject* p);
SchemeObject* s_number_p(SchemeObject* p);
SchemeObject* s_vector_p(SchemeObject* p);
SchemeObject* s_pair_p(SchemeObject* p);
SchemeObject* s_null_p(SchemeObject* p);
SchemeObject* s_symbol_p(SchemeObject* p);

SchemeObject* s_assoc(SchemeObject* obj, SchemeObject* alist);
SchemeObject* s_assq(SchemeObject* obj, SchemeObject* alist);
SchemeObject* s_assv(SchemeObject* obj, SchemeObject* alist);

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

SchemeObject* s_cons(SchemeObject* car, SchemeObject* cdr);
SchemeObject* s_set_car_e(SchemeObject* p, SchemeObject* o);
SchemeObject* s_set_cdr_e(SchemeObject* p, SchemeObject* o);
SchemeObject* s_append(SchemeObject* args);
SchemeObject* s_reverse(SchemeObject* l);
SchemeObject* s_length(SchemeObject* l);
SchemeObject* s_list(SchemeObject* args);
SchemeObject* s_list_tail(SchemeObject* l, SchemeObject* k);
SchemeObject* s_list_ref(SchemeObject* l, SchemeObject* k);
SchemeObject* s_member(SchemeObject* obj, SchemeObject* p);
SchemeObject* s_memq(SchemeObject* obj, SchemeObject* p);
SchemeObject* s_memv(SchemeObject* obj, SchemeObject* p);

// Vector stuff
SchemeObject* s_make_vector(SchemeObject* count, SchemeObject* obj);
SchemeObject* s_vector(SchemeObject* args);
SchemeObject* s_vector_length(SchemeObject* v);
SchemeObject* s_list_2_vector(SchemeObject* list);
SchemeObject* s_vector_2_list(SchemeObject* v);
SchemeObject* s_vector_ref(SchemeObject* v, SchemeObject* i);
SchemeObject* s_vector_set_e(SchemeObject* vec, SchemeObject* index, SchemeObject* val);
SchemeObject* s_vector_fill_e(SchemeObject* vec, SchemeObject* fill);

// Char stuff
SchemeObject* s_char_upcase(SchemeObject* c);
SchemeObject* s_char_downcase(SchemeObject* c);
SchemeObject* s_char_alphabetic_p(SchemeObject* c);
SchemeObject* s_char_numeric_p(SchemeObject* c);
SchemeObject* s_char_whitespace_p(SchemeObject* c);
SchemeObject* s_char_upper_case_p(SchemeObject* c);
SchemeObject* s_char_lower_case_p(SchemeObject* c);
SchemeObject* s_integer_2_char(SchemeObject* i);
SchemeObject* s_char_2_integer(SchemeObject* c);

// Math stuff
SchemeObject* s_equal(SchemeObject* p);
SchemeObject* s_less(SchemeObject* p);
SchemeObject* s_greater(SchemeObject* p);
SchemeObject* s_less_equal(SchemeObject* p);
SchemeObject* s_greater_equal(SchemeObject* p);
SchemeObject* s_plus(SchemeObject* l);
SchemeObject* s_minus(SchemeObject* n, SchemeObject* rst);
SchemeObject* s_mult(SchemeObject* l);
SchemeObject* s_divide(SchemeObject* n, SchemeObject* rst);
SchemeObject* s_sqrt(SchemeObject* n);
SchemeObject* s_abs(SchemeObject* n);
SchemeObject* s_sin(SchemeObject* n);
SchemeObject* s_cos(SchemeObject* n);
SchemeObject* s_asin(SchemeObject* n);
SchemeObject* s_acos(SchemeObject* n);
SchemeObject* s_tan(SchemeObject* n);
SchemeObject* s_atan(SchemeObject* y, SchemeObject* x);
SchemeObject* s_expt(SchemeObject* y, SchemeObject* x);
SchemeObject* s_exp(SchemeObject* n);
SchemeObject* s_log(SchemeObject* n);
SchemeObject* s_min(SchemeObject* n, SchemeObject* l);
SchemeObject* s_max(SchemeObject* n, SchemeObject* l);
SchemeObject* s_gcd(SchemeObject* l);
SchemeObject* s_lcm(SchemeObject* l);
SchemeObject* s_round(SchemeObject* n);
SchemeObject* s_floor(SchemeObject* n);
SchemeObject* s_ceiling(SchemeObject* n);
SchemeObject* s_truncate(SchemeObject* n);
SchemeObject* s_even_p(SchemeObject* n);
SchemeObject* s_odd_p(SchemeObject* n);
SchemeObject* s_zero_p(SchemeObject* n);
SchemeObject* s_negative_p(SchemeObject* n);
SchemeObject* s_positive_p(SchemeObject* n);
SchemeObject* s_integer_p(SchemeObject* n);
SchemeObject* s_complex_p(SchemeObject* n);
SchemeObject* s_rational_p(SchemeObject* n);
SchemeObject* s_real_p(SchemeObject* n);
SchemeObject* s_exact_p(SchemeObject* n);
SchemeObject* s_inexact_p(SchemeObject* n);
SchemeObject* s_quotient(SchemeObject* n1, SchemeObject* n2);
SchemeObject* s_remainder(SchemeObject* n1, SchemeObject* n2);
SchemeObject* s_modulo(SchemeObject* n1, SchemeObject* n2);

// String stuff
SchemeObject* s_make_string(SchemeObject* len, SchemeObject* chr);
SchemeObject* s_string(SchemeObject* chars);
SchemeObject* s_string_length(SchemeObject* s);
SchemeObject* s_string_ref(SchemeObject* s, SchemeObject* i);
SchemeObject* s_string_set_e(SchemeObject* str, SchemeObject* i, SchemeObject* chr);
SchemeObject* s_symbol_2_string(SchemeObject* symbol);
SchemeObject* s_string_2_symbol(SchemeObject* s);
SchemeObject* s_number_2_string(SchemeObject* n, SchemeObject* base);
SchemeObject* s_string_2_number(SchemeObject* s, SchemeObject* base);
SchemeObject* s_string_append(SchemeObject* strings);
SchemeObject* s_string_copy(SchemeObject* string);
SchemeObject* s_string_2_list(SchemeObject* s);
SchemeObject* s_list_2_string(SchemeObject* p);
SchemeObject* s_substring(SchemeObject* str, SchemeObject* start, SchemeObject* end);
SchemeObject* s_symgen();


// Input and output stuff
SchemeObject* s_current_input_port();
SchemeObject* s_current_output_port();
SchemeObject* s_input_port_p(SchemeObject* o);
SchemeObject* s_output_port_p(SchemeObject* o);
SchemeObject* s_eof_object_p(SchemeObject* o);
SchemeObject* s_open_input_file(SchemeObject* s_filename);
SchemeObject* s_open_output_file(SchemeObject* s_filename);
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

extern SchemeObject* if_symbol;
extern SchemeObject* cond_symbol;
extern SchemeObject* apply_symbol;
extern SchemeObject* else_symbol;
extern SchemeObject* ergo_symbol;
extern SchemeObject* case_symbol;
extern SchemeObject* do_symbol;
extern SchemeObject* let_symbol;
extern SchemeObject* letstar_symbol;
extern SchemeObject* letrec_symbol;
extern SchemeObject* begin_symbol;
extern SchemeObject* and_symbol;
extern SchemeObject* or_symbol;
extern SchemeObject* lambda_symbol;
extern SchemeObject* quote_symbol;
extern SchemeObject* quasiquote_symbol;
extern SchemeObject* unquote_symbol;
extern SchemeObject* unquote_splicing_symbol;
extern SchemeObject* define_symbol;
extern SchemeObject* define_macro;
extern SchemeObject* set_e_symbol;
extern SchemeObject* unnamed_symbol;

#endif
