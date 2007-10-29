
#ifndef SCHEME_SCHEME_H
#define SCHEME_SCHEME_H

#include <iostream>
#include <sstream>
#include <string>
#include "objects.h"
#include "heap.h"

using namespace std;

typedef vector<SchemeObject*> SchemeStack;

class Scheme {
    public:
        Scheme();
        SchemeObject* eval(wstring code, SchemeObject* envt = NULL);
        SchemeObject* eval(wistream* code, SchemeObject* envt = NULL);

        // For assigning variables in a frame (default top-level)
        void assign(wstring variable, double value, SchemeObject* envt = NULL);
        void assign(wstring variable, wstring value, SchemeObject* envt = NULL);
        void assign(wstring variable, bool value, SchemeObject* envt = NULL);
        void assign(wstring variable, SchemeObject* value, SchemeObject* envt = NULL);

        // Look up in frame (default top-level)
        SchemeObject* lookup(wstring variable, SchemeObject* envt = NULL);
        SchemeObject* lookup(SchemeObject* symbol, SchemeObject* envt = NULL);
        SchemeObject* lookupOrFail(SchemeObject* symbol, SchemeObject* envt = NULL);
        
        // For assigning built-in functions at top-level-frame
	void assign(wstring variable, int req, int opt, int rst, SchemeObject* (*fn)(), SchemeObject* b = NULL);

        // For calls from embedding code
        SchemeObject* callProcedure_1(SchemeObject* s_proc, SchemeObject*);
        SchemeObject* callProcedure_2(SchemeObject* s_proc, SchemeObject*, SchemeObject*);
        SchemeObject* callProcedure_3(SchemeObject* s_proc, SchemeObject*, SchemeObject*, SchemeObject*);
        
        // Force a garbage collection
        void forceGarbageCollection();

    private:
};

class scheme_exception {
    public: 
	scheme_exception(wstring error);
	scheme_exception(wchar_t* procname, wstring error);
	scheme_exception(uint32_t line, wstring error);
        wstring toString();
    private:		
	wstring str;
        wchar_t* procname;
        uint32_t line;
};

#define wrong_type_arg(procname, argnum, arg) { \
        wostringstream ss;                                 \
        ss << L"Wrong argument-type in position ";         \
        ss << argnum;                                     \
        ss << L" in call to ";                             \
        ss << wstring(procname);                           \
        ss << L": " << (arg)->toString();                    \
        throw scheme_exception(ss.str());                 \
}

#define assert_arg_type(procname, argnum, test_fn, arg) { \
    if ((test_fn)(arg) == S_FALSE) {                      \
        wrong_type_arg(procname, argnum, (arg));            \
    }                                                     \
}

#define assert_arg_pair_type(procname, argnum, arg) {     \
    if (i_pair_p(arg) == S_FALSE) {                       \
        wostringstream ss;                                 \
        ss << "Wrong argument-type (expecting pair) in position ";         \
        ss << argnum;                                     \
        ss << " in call to ";                             \
        ss << wstring(procname);                           \
        ss << ": " << (arg)->toString();                    \
        throw scheme_exception(ss.str());                 \
    }                                                     \
}

#define assert_arg_procedure_type(procname, argnum, arg) {     \
    if (i_procedure_p(arg) == S_FALSE) {                       \
        wostringstream ss;                                 \
        ss << "Wrong argument-type (expecting procedure) in position ";         \
        ss << argnum;                                     \
        ss << " in call to ";                             \
        ss << wstring(procname);                           \
        ss << ": " << (arg)->toString();                    \
        throw scheme_exception(ss.str());                 \
    }                                                     \
}


#define assert_arg_number_type(procname, argnum, arg) {   \
    if (i_number_p(arg) == S_FALSE) {                     \
        wostringstream ss;                                 \
        ss << "Wrong argument-type (expecting number) in position ";         \
        ss << argnum;                                     \
        ss << " in call to ";                             \
        ss << wstring(procname);                           \
        ss << ": " << (arg)->toString();                    \
        throw scheme_exception(ss.str());                 \
    }                                                     \
}

#define assert_arg_int_type(procname, argnum, arg) {   \
    if (s_integer_p(arg) == S_FALSE) {                     \
        wostringstream ss;                                 \
        ss << "Wrong argument-type (expecting number) in position ";         \
        ss << argnum;                                     \
        ss << " in call to ";                             \
        ss << wstring(procname);                           \
        ss << ": " << (arg)->toString();                    \
        throw scheme_exception(ss.str());                 \
    }                                                     \
}

#define assert_arg_symbol_type(procname, argnum, arg) {   \
    if (s_symbol_p(arg) == S_FALSE) {                     \
        wostringstream ss;                                 \
        ss << "Wrong argument-type (expecting symbol) in position ";         \
        ss << argnum;                                     \
        ss << " in call to ";                             \
        ss << procname;                                   \
        ss << ": " << (arg)->toString();                  \
        throw scheme_exception(ss.str());                 \
    }                                                     \
}


#define assert_arg_int_in_range(procname, argnum, arg, from, to) {    \
    assert_arg_type(procname, argnum, s_integer_p, arg);              \
    int n = scm2int(arg);                                             \
    if (n < from || n > to) {                                         \
        wostringstream ss;                                             \
        ss << "Integer out of range " << from << " to " << to;        \
        ss << " in position " << argnum;                              \
        ss << " in call to " << procname;                             \
        ss << ": " << (arg)->toString();                                \
        throw scheme_exception(ss.str());                             \
    }                                                                 \
}

#define assert_arg_positive_int(procname, argnum, arg) {           \
    assert_arg_type(procname, argnum, s_integer_p, arg);           \
    int n = scm2int(arg);                                          \
    if (n < 0) {                                                   \
        wostringstream ss;                                          \
        ss << "Negative argument in to position " << argnum;       \
        ss << " in call to " << wstring(procname);                  \
        ss << ": " << (arg)->toString();                             \
        throw scheme_exception(ss.str());                          \
    }                                                              \
}


#define assert_arg_not_immutable(procname, argnum, arg) {   \
    if (arg->immutable()) {                                 \
        wostringstream ss;                                   \
        ss << "Can't modify immutable object in position "; \
        ss << argnum;                                       \
        ss << " in call to " << wstring(procname);           \
        ss << ": " << (arg)->toString();                      \
        throw scheme_exception(ss.str());                   \
    }                                                       \
}

#define assert_non_atom_type(procname, argnum, arg) {   \
    if (i_pair_p(arg) == S_FALSE && i_null_p(arg) == S_FALSE) { \
        wostringstream ss;                                 \
        ss << "Wrong argument-type (expecting non-atom) in position ";         \
        ss << argnum;                                     \
        ss << " in call to ";                             \
        ss << wstring(procname);                           \
        ss << ": " << (arg)->toString();                    \
        throw scheme_exception(ss.str());                 \
    }                                                     \
}

#define assert_arg_wrapped_type(procname, argnum, arg,subtype) {   \
    if (i_wrapped_object_p(arg,subtype) == S_FALSE) {     \
        wostringstream ss;                                 \
        ss << "Wrong argument-type (expecting wrapped object subtype" << subtype << ") in position ";         \
        ss << argnum;                                     \
        ss << " in call to ";                             \
        ss << wstring(procname);                           \
        ss << ": " << (arg)->toString();                    \
        throw scheme_exception(ss.str());                 \
    }                                                     \
}


// Scheme constants
extern SchemeObject* S_TRUE;
extern SchemeObject* S_FALSE;
extern SchemeObject* S_UNSPECIFIED;
extern SchemeObject* S_EMPTY_LIST;
extern SchemeObject* S_ZERO;
extern SchemeObject* S_ONE;
extern SchemeObject* S_TWO;
extern SchemeObject* S_NUMBERS[];

// Conversion macros
#define scm2int(o)     (int((o)->value))
#define scm2double(o)  ((o)->value)
#define scm2string(o)  (wstring((o)->str))
#define scm2char(o)    ((o)->c)
#define scm2bool(o)    ((o) != S_FALSE)
#define bool2scm(b)    ((b) ? S_TRUE : S_FALSE)
#define string2scm(s)  (SchemeObject::createString(s.c_str()))
#define cstr2scm(s)    (SchemeObject::createString(s))
#define char2scm(c)    (SchemeObject::createChar(c))
#define int2scm(n)     (((n) < 10 && (n) >= 0) ? S_NUMBERS[n] : SchemeObject::createNumber(n))
#define double2scm(n)  (SchemeObject::createNumber(n))

class SchemeAppendableList {
    public:
        SchemeAppendableList() {
            list = S_EMPTY_LIST;
            tail = S_EMPTY_LIST;
        };
        void add(SchemeObject* o) {
            SchemeObject* pair = i_cons(o, S_EMPTY_LIST);
            if (list == S_EMPTY_LIST) {
                list = pair;
                tail = pair;            
            } else {
                i_set_cdr_e(tail, pair);
                tail = pair;
            }        
        };
        SchemeObject* list;
        
    private:
        SchemeObject* tail;
};


// Declaration of scheme procedures
SchemeObject* s_equal_p(SchemeObject* a, SchemeObject* b);
SchemeObject* s_eq_p(SchemeObject* a, SchemeObject* b);
SchemeObject* s_eqv_p(SchemeObject* a, SchemeObject* b);
SchemeObject* s_not(SchemeObject*);

SchemeObject* s_call_cc(SchemeObject* proc);

SchemeObject* s_apply(int num, SchemeStack::iterator stack);
SchemeObject* s_null_environment(SchemeObject* version);
SchemeObject* s_scheme_report_environment(SchemeObject* version);
SchemeObject* s_interaction_environment(SchemeObject* version);
SchemeObject* s_eval(SchemeObject* expression, SchemeObject* environment);

SchemeObject* s_map(int num, SchemeStack::iterator stack);
SchemeObject* s_for_each(int num, SchemeStack::iterator stack);

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
SchemeObject* s_caar(SchemeObject* o);
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
SchemeObject* s_caaaar(SchemeObject* o);
SchemeObject* s_caaadr(SchemeObject* o);
SchemeObject* s_caadar(SchemeObject* o);
SchemeObject* s_caaddr(SchemeObject* o);
SchemeObject* s_cadaar(SchemeObject* o);
SchemeObject* s_cadadr(SchemeObject* o);
SchemeObject* s_caddar(SchemeObject* o);
SchemeObject* s_cadddr(SchemeObject* o);
SchemeObject* s_cdaaar(SchemeObject* o);
SchemeObject* s_cdaadr(SchemeObject* o);
SchemeObject* s_cdadar(SchemeObject* o);
SchemeObject* s_cdaddr(SchemeObject* o);
SchemeObject* s_cddaar(SchemeObject* o);
SchemeObject* s_cddadr(SchemeObject* o);
SchemeObject* s_cdddar(SchemeObject* o);
SchemeObject* s_cddddr(SchemeObject* o);

SchemeObject* s_cons(SchemeObject* car, SchemeObject* cdr);
SchemeObject* s_set_car_e(SchemeObject* p, SchemeObject* o);
SchemeObject* s_set_cdr_e(SchemeObject* p, SchemeObject* o);
SchemeObject* s_append(int num, SchemeStack::iterator stack);
SchemeObject* s_reverse(SchemeObject* l);
SchemeObject* s_length(SchemeObject* l);
SchemeObject* s_list(int num, SchemeStack::iterator stack);
SchemeObject* s_list_tail(SchemeObject* l, SchemeObject* k);
SchemeObject* s_list_ref(SchemeObject* l, SchemeObject* k);
SchemeObject* s_member(SchemeObject* obj, SchemeObject* p);
SchemeObject* s_memq(SchemeObject* obj, SchemeObject* p);
SchemeObject* s_memv(SchemeObject* obj, SchemeObject* p);

// Vector stuff
SchemeObject* s_make_vector(SchemeObject* count, SchemeObject* obj);
SchemeObject* s_vector(int num, SchemeStack::iterator stack);
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
SchemeObject* s_char_equal_p(int num, SchemeStack::iterator args);
SchemeObject* s_char_less_p(int num, SchemeStack::iterator args);
SchemeObject* s_char_greater_p(int num, SchemeStack::iterator args);
SchemeObject* s_char_less_equal_p(int num, SchemeStack::iterator args);
SchemeObject* s_char_greater_equal_p(int num, SchemeStack::iterator args);
SchemeObject* s_char_ci_equal_p(int num, SchemeStack::iterator args);
SchemeObject* s_char_ci_less_p(int num, SchemeStack::iterator args);
SchemeObject* s_char_ci_greater_p(int num, SchemeStack::iterator args);
SchemeObject* s_char_ci_less_equal_p(int num, SchemeStack::iterator args);
SchemeObject* s_char_ci_greater_equal_p(int num, SchemeStack::iterator args);

// Math stuff
SchemeObject* s_equal(int num, SchemeStack::iterator stack);
SchemeObject* s_less(int num, SchemeStack::iterator stack);
SchemeObject* s_greater(int num, SchemeStack::iterator stack);
SchemeObject* s_less_equal(int num, SchemeStack::iterator stack);
SchemeObject* s_greater_equal(int num, SchemeStack::iterator stack);
SchemeObject* s_plus(int num, SchemeStack::iterator stack);
SchemeObject* s_minus(int num, SchemeStack::iterator stack);
SchemeObject* s_mult(int num, SchemeStack::iterator stack);
SchemeObject* s_divide(int num, SchemeStack::iterator stack);
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
SchemeObject* s_min(int num, SchemeStack::iterator stack);
SchemeObject* s_max(int num, SchemeStack::iterator stack);
SchemeObject* s_gcd(int num, SchemeStack::iterator stack);
SchemeObject* s_lcm(int num, SchemeStack::iterator stack);
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
SchemeObject* s_string(int num, SchemeStack::iterator stack);
SchemeObject* s_string_length(SchemeObject* s);
SchemeObject* s_string_ref(SchemeObject* s, SchemeObject* i);
SchemeObject* s_string_set_e(SchemeObject* str, SchemeObject* i, SchemeObject* chr);
SchemeObject* s_symbol_2_string(SchemeObject* symbol);
SchemeObject* s_string_2_symbol(SchemeObject* s);
SchemeObject* s_number_2_string(SchemeObject* n, SchemeObject* base);
SchemeObject* s_string_2_number(SchemeObject* s, SchemeObject* base);
SchemeObject* s_string_append(int num, SchemeStack::iterator stack);
SchemeObject* s_string_copy(SchemeObject* string);
SchemeObject* s_string_2_list(SchemeObject* s);
SchemeObject* s_list_2_string(SchemeObject* p);
SchemeObject* s_substring(SchemeObject* str, SchemeObject* start, SchemeObject* end);
SchemeObject* s_string_equal_p(int num, SchemeStack::iterator args);
SchemeObject* s_string_less_p(int num, SchemeStack::iterator args);
SchemeObject* s_string_greater_p(int num, SchemeStack::iterator args);
SchemeObject* s_string_less_equal_p(int num, SchemeStack::iterator args);
SchemeObject* s_string_greater_equal_p(int num, SchemeStack::iterator args);
SchemeObject* s_string_ci_equal_p(int num, SchemeStack::iterator args);
SchemeObject* s_string_ci_less_p(int num, SchemeStack::iterator args);
SchemeObject* s_string_ci_greater_p(int num, SchemeStack::iterator args);
SchemeObject* s_string_ci_less_equal_p(int num, SchemeStack::iterator args);
SchemeObject* s_string_ci_greater_equal_p(int num, SchemeStack::iterator args);
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
SchemeObject* s_load(SchemeObject* filename);

// My extensions
SchemeObject* s_find_duplicate(SchemeObject* l);

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
