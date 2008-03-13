
#ifndef SCHEME_SCHEME_H
#define SCHEME_SCHEME_H

#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include "objects.h"
#include "heap.h"

using namespace std;

typedef vector<SchemeObject*> SchemeStack;

class Interpreter;
class Parser;

class Scheme {
    public:
        Scheme();
        SchemeObject* eval(wstring code, SchemeObject* envt = NULL);
        SchemeObject* eval(SchemeObject* port, SchemeObject* envt = NULL);

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
        SchemeObject* callProcedure_0(SchemeObject* s_proc);
        SchemeObject* callProcedure_1(SchemeObject* s_proc, SchemeObject*);
        SchemeObject* callProcedure_2(SchemeObject* s_proc, SchemeObject*, SchemeObject*);
        SchemeObject* callProcedure_3(SchemeObject* s_proc, SchemeObject*, SchemeObject*, SchemeObject*);
        SchemeObject* callProcedure_n(SchemeObject* s_proc, SchemeObject* args_list);
        
        // Globals that we want to keep away from the garbage collector
        void keepForever(SchemeObject*);
        
        // Force a garbage collection
        void forceGarbageCollection();
        
        // Get interaction environment
        SchemeObject* getInteractionEnvironment();

        SchemeObject* current_input_port;
        SchemeObject* current_output_port;
        
        SchemeObject* if_symbol;
        SchemeObject* cond_symbol;
        SchemeObject* apply_symbol;
        SchemeObject* else_symbol;
        SchemeObject* ergo_symbol;
        SchemeObject* case_symbol;
        SchemeObject* do_symbol;
        SchemeObject* let_symbol;
        SchemeObject* letstar_symbol;
        SchemeObject* letrec_symbol;
        SchemeObject* begin_symbol;
        SchemeObject* and_symbol;
        SchemeObject* or_symbol;
        SchemeObject* lambda_symbol;
        SchemeObject* lambda_symbol_short;
        SchemeObject* quote_symbol;
        SchemeObject* quasiquote_symbol;
        SchemeObject* unquote_symbol;
        SchemeObject* unquote_splicing_symbol;
        SchemeObject* define_symbol;
        SchemeObject* define_macro_symbol;
        SchemeObject* set_e_symbol;
        SchemeObject* define_syntax_symbol;
        SchemeObject* unnamed_symbol;
        SchemeObject* let_syntax_symbol;
        SchemeObject* letrec_syntax_symbol;
        SchemeObject* ellipsis_symbol;
        
        SchemeObject* null_environment;
        SchemeObject* scheme_report_environment;
        SchemeObject* interaction_environment;
        
    public:
   	    // Used for testing only
        Interpreter* getInterpreter();
        
        // Used by (read) only
        Parser* getParser() { return parser; };
        
    private:
        Interpreter* interpreter;
        Parser* parser;    
};

extern SchemeObject* S_UNSPECIFIED;
extern SchemeObject* S_EMPTY_LIST;
extern SchemeObject* S_EOF;
extern SchemeObject* S_TRUE;
extern SchemeObject* S_FALSE;

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

#define assert_arg_type(scheme,procname, argnum, test_fn, arg) { \
    if ((test_fn)(scheme,arg) == S_FALSE) {                      \
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

#define assert_arg_vector_type(procname, argnum, arg) {     \
    if (i_vector_p(arg) == S_FALSE) {                       \
        wostringstream ss;                                 \
        ss << "Wrong argument-type (expecting vector) in position ";         \
        ss << argnum;                                     \
        ss << " in call to ";                             \
        ss << wstring(procname);                           \
        ss << ": " << (arg)->toString();                    \
        throw scheme_exception(ss.str());                 \
    }                                                     \
}

#define assert_arg_string_type(procname, argnum, arg) {     \
    if (i_string_p(arg) == S_FALSE) {                       \
        wostringstream ss;                                 \
        ss << "Wrong argument-type (expecting string) in position ";         \
        ss << argnum;                                     \
        ss << " in call to ";                             \
        ss << wstring(procname);                           \
        ss << ": " << (arg)->toString();                    \
        throw scheme_exception(ss.str());                 \
    }                                                     \
}

#define assert_arg_char_type(procname, argnum, arg) {     \
    if (i_char_p(arg) == S_FALSE) {                       \
        wostringstream ss;                                 \
        ss << "Wrong argument-type (expecting char) in position ";         \
        ss << argnum;                                     \
        ss << " in call to ";                             \
        ss << wstring(procname);                           \
        ss << ": " << (arg)->toString();                    \
        throw scheme_exception(ss.str());                 \
    }                                                     \
}

#define assert_arg_symbol_type(procname, argnum, arg) {     \
    if (i_symbol_p(arg) == S_FALSE) {                       \
        wostringstream ss;                                 \
        ss << "Wrong argument-type (expecting symbol) in position ";         \
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
    if (i_integer_p(arg) == S_FALSE) {                     \
        wostringstream ss;                                 \
        ss << "Wrong argument-type (expecting integer) in position ";         \
        ss << argnum;                                     \
        ss << " in call to ";                             \
        ss << wstring(procname);                           \
        ss << ": " << (arg)->toString();                    \
        throw scheme_exception(ss.str());                 \
    }                                                     \
}

#define assert_arg_complex_type(procname, argnum, arg) {   \
    if (i_complex_p(arg) == S_FALSE) {                     \
        wostringstream ss;                                 \
        ss << "Wrong argument-type (expecting complex) in position ";         \
        ss << argnum;                                     \
        ss << " in call to ";                             \
        ss << wstring(procname);                           \
        ss << ": " << (arg)->toString();                    \
        throw scheme_exception(ss.str());                 \
    }                                                     \
}

#define assert_arg_real_type(procname, argnum, arg) {   \
    if (i_real_p(arg) == S_FALSE) {                     \
        wostringstream ss;                                 \
        ss << "Wrong argument-type (expecting real) in position ";         \
        ss << argnum;                                     \
        ss << " in call to ";                             \
        ss << wstring(procname);                           \
        ss << ": " << (arg)->toString();                    \
        throw scheme_exception(ss.str());                 \
    }                                                     \
}

#define assert_arg_int_in_range(procname, argnum, arg, from, to) {    \
    assert_arg_int_type(procname, argnum, arg);              \
    int64_t n = scm2int(arg);                                             \
    if (n < from || n > to) {                                         \
        wostringstream ss;                                             \
        ss << "Integer out of range [" << from << ".." << to;        \
        ss << "] in position " << argnum;                              \
        ss << " in call to " << procname;                             \
        ss << ": " << (arg)->toString();                                \
        throw scheme_exception(ss.str());                             \
    }                                                                 \
}

#define assert_arg_positive_int(procname, argnum, arg) {           \
    assert_arg_int_type(procname, argnum, arg);           \
    int64_t n = scm2int(arg);                                          \
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
        ss << "Wrong argument-type (expecting wrapped object subtype " << subtype << ") in position ";         \
        ss << argnum;                                     \
        ss << " in call to ";                             \
        ss << wstring(procname);                           \
        ss << ": " << (arg)->toString();                    \
        throw scheme_exception(ss.str());                 \
    }                                                     \
}

#define assert_arg_procedure_that_take(procname, argnum, arg, parity) {   \
    if (i_procedure_p(arg) == S_FALSE || \
        !(arg->rest() > 0 || arg->req() == parity || arg->req()+arg->opt() == parity)) {    \
        wostringstream ss;                                 \
        ss << "Wrong argument-type (expecting procedure that takes "; \
        ss << parity << " args) in position ";         \
        ss << argnum;                                     \
        ss << " in call to ";                             \
        ss << wstring(procname);                           \
        ss << ": " << (arg)->toString();                    \
        throw scheme_exception(ss.str());                 \
    }                                                     \
}



// Conversion macros
#define scm2rational(o) ((o)->rationalValue())
#define scm2complex(o)  ((o)->complexValue())
#define scm2double(o)   ((o)->realValue())
#define scm2int(o)      ((o)->integerValue())
#define scm2string(o)   (wstring((o)->str))
#define scm2char(o)     ((o)->c)
#define scm2bool(o)     ((o) != S_FALSE)
#define bool2scm(b)     ((b) ? S_TRUE : S_FALSE)
#define string2scm(s)   (SchemeObject::createString(s.c_str()))
#define cstr2scm(s)     (SchemeObject::createString(s))
#define char2scm(c)     (SchemeObject::createChar(c))
#define int2scm(n)      (SchemeObject::createIntegerNumber(n))
#define uint2scm(n)     (SchemeObject::createIntegerNumber(n))
#define complex2scm(n)  (SchemeObject::createComplexNumber(n))
#define rational2scm(n) (SchemeObject::createRationalNumber(n))
#define double2scm(n)   (SchemeObject::createRealNumber(n))

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
SchemeObject* s_equal_p(Scheme* scheme, SchemeObject* a, SchemeObject* b);
SchemeObject* s_eq_p(Scheme* scheme, SchemeObject* a, SchemeObject* b);
SchemeObject* s_eqv_p(Scheme* scheme, SchemeObject* a, SchemeObject* b);
SchemeObject* s_not(Scheme* scheme, SchemeObject*);

SchemeObject* s_call_cc(Scheme* scheme, SchemeObject* proc);

SchemeObject* s_apply(Scheme* scheme, int num, SchemeStack::iterator stack);
SchemeObject* s_null_environment(Scheme* scheme, SchemeObject* version);
SchemeObject* s_scheme_report_environment(Scheme* scheme, SchemeObject* version);
SchemeObject* s_interaction_environment(Scheme* scheme, SchemeObject* version);
SchemeObject* s_eval(Scheme* scheme, SchemeObject* expression, SchemeObject* environment);

SchemeObject* s_map(Scheme* scheme, int num, SchemeStack::iterator stack);
SchemeObject* s_for_each(Scheme* scheme, int num, SchemeStack::iterator stack);

SchemeObject* s_boolean_p(Scheme* scheme, SchemeObject* o);
SchemeObject* s_string_p(Scheme* scheme, SchemeObject* o);
SchemeObject* s_char_p(Scheme* scheme, SchemeObject* o);
SchemeObject* s_procedure_p(Scheme* scheme, SchemeObject* o);
SchemeObject* s_list_p(Scheme* scheme, SchemeObject* p);
SchemeObject* i_list_p(SchemeObject* p);
SchemeObject* s_vector_p(Scheme* scheme, SchemeObject* p);
SchemeObject* s_pair_p(Scheme* scheme, SchemeObject* p);
SchemeObject* s_null_p(Scheme* scheme, SchemeObject* p);
SchemeObject* s_symbol_p(Scheme* scheme, SchemeObject* p);

SchemeObject* s_car(Scheme* scheme, SchemeObject* o);
SchemeObject* s_cdr(Scheme* scheme, SchemeObject* o);
SchemeObject* s_caar(Scheme* scheme, SchemeObject* o);
SchemeObject* s_cadr(Scheme* scheme, SchemeObject* o);
SchemeObject* s_cdar(Scheme* scheme, SchemeObject* o);
SchemeObject* s_cddr(Scheme* scheme, SchemeObject* o);
SchemeObject* s_caaar(Scheme* scheme, SchemeObject* o);
SchemeObject* s_caadr(Scheme* scheme, SchemeObject* o);
SchemeObject* s_cadar(Scheme* scheme, SchemeObject* o);
SchemeObject* s_caddr(Scheme* scheme, SchemeObject* o);
SchemeObject* s_cdaar(Scheme* scheme, SchemeObject* o);
SchemeObject* s_cdadr(Scheme* scheme, SchemeObject* o);
SchemeObject* s_cddar(Scheme* scheme, SchemeObject* o);
SchemeObject* s_cdddr(Scheme* scheme, SchemeObject* o);
SchemeObject* s_caaaar(Scheme* scheme, SchemeObject* o);
SchemeObject* s_caaadr(Scheme* scheme, SchemeObject* o);
SchemeObject* s_caadar(Scheme* scheme, SchemeObject* o);
SchemeObject* s_caaddr(Scheme* scheme, SchemeObject* o);
SchemeObject* s_cadaar(Scheme* scheme, SchemeObject* o);
SchemeObject* s_cadadr(Scheme* scheme, SchemeObject* o);
SchemeObject* s_caddar(Scheme* scheme, SchemeObject* o);
SchemeObject* s_cadddr(Scheme* scheme, SchemeObject* o);
SchemeObject* s_cdaaar(Scheme* scheme, SchemeObject* o);
SchemeObject* s_cdaadr(Scheme* scheme, SchemeObject* o);
SchemeObject* s_cdadar(Scheme* scheme, SchemeObject* o);
SchemeObject* s_cdaddr(Scheme* scheme, SchemeObject* o);
SchemeObject* s_cddaar(Scheme* scheme, SchemeObject* o);
SchemeObject* s_cddadr(Scheme* scheme, SchemeObject* o);
SchemeObject* s_cdddar(Scheme* scheme, SchemeObject* o);
SchemeObject* s_cddddr(Scheme* scheme, SchemeObject* o);

SchemeObject* s_cons(Scheme* scheme, SchemeObject* car, SchemeObject* cdr);
SchemeObject* s_set_car_e(Scheme* scheme, SchemeObject* p, SchemeObject* o);
SchemeObject* s_set_cdr_e(Scheme* scheme, SchemeObject* p, SchemeObject* o);
SchemeObject* s_append(Scheme* scheme, int num, SchemeStack::iterator stack);
SchemeObject* s_reverse(Scheme* scheme, SchemeObject* l);
SchemeObject* s_length(Scheme* scheme, SchemeObject* l);
int64_t       i_length(SchemeObject* p);
SchemeObject* s_list(Scheme* scheme, int num, SchemeStack::iterator stack);
SchemeObject* i_list_tail(SchemeObject* l, SchemeObject* k);
SchemeObject* s_list_tail(Scheme* scheme, SchemeObject* l, SchemeObject* k);
SchemeObject* i_list_ref(SchemeObject* l, SchemeObject* k);
SchemeObject* s_list_ref(Scheme* scheme, SchemeObject* l, SchemeObject* k);

// Vector stuff
SchemeObject* s_make_vector(Scheme* scheme, SchemeObject* count, SchemeObject* obj);
SchemeObject* s_vector(Scheme* scheme, int num, SchemeStack::iterator stack);
SchemeObject* s_vector_length(Scheme* scheme, SchemeObject* v);
SchemeObject* s_list_2_vector(Scheme* scheme, SchemeObject* list);
SchemeObject* s_vector_2_list(Scheme* scheme, SchemeObject* v);
SchemeObject* s_vector_ref(Scheme* scheme, SchemeObject* v, SchemeObject* i);
SchemeObject* s_vector_set_e(Scheme* scheme, SchemeObject* vec, SchemeObject* index, SchemeObject* val);
SchemeObject* s_vector_fill_e(Scheme* scheme, SchemeObject* vec, SchemeObject* fill);

// Char stuff
SchemeObject* s_char_upcase(Scheme* scheme, SchemeObject* c);
SchemeObject* s_char_downcase(Scheme* scheme, SchemeObject* c);
SchemeObject* s_char_alphabetic_p(Scheme* scheme, SchemeObject* c);
SchemeObject* s_char_numeric_p(Scheme* scheme, SchemeObject* c);
SchemeObject* s_char_whitespace_p(Scheme* scheme, SchemeObject* c);
SchemeObject* s_char_upper_case_p(Scheme* scheme, SchemeObject* c);
SchemeObject* s_char_lower_case_p(Scheme* scheme, SchemeObject* c);
SchemeObject* s_integer_2_char(Scheme* scheme, SchemeObject* i);
SchemeObject* s_char_2_integer(Scheme* scheme, SchemeObject* c);
SchemeObject* s_char_equal_p(Scheme* scheme, int num, SchemeStack::iterator args);
SchemeObject* s_char_less_p(Scheme* scheme, int num, SchemeStack::iterator args);
SchemeObject* s_char_greater_p(Scheme* scheme, int num, SchemeStack::iterator args);
SchemeObject* s_char_less_equal_p(Scheme* scheme, int num, SchemeStack::iterator args);
SchemeObject* s_char_greater_equal_p(Scheme* scheme, int num, SchemeStack::iterator args);
SchemeObject* s_char_ci_equal_p(Scheme* scheme, int num, SchemeStack::iterator args);
SchemeObject* s_char_ci_less_p(Scheme* scheme, int num, SchemeStack::iterator args);
SchemeObject* s_char_ci_greater_p(Scheme* scheme, int num, SchemeStack::iterator args);
SchemeObject* s_char_ci_less_equal_p(Scheme* scheme, int num, SchemeStack::iterator args);
SchemeObject* s_char_ci_greater_equal_p(Scheme* scheme, int num, SchemeStack::iterator args);

// String stuff
SchemeObject* s_make_string(Scheme* scheme, SchemeObject* len, SchemeObject* chr);
SchemeObject* s_string(Scheme* scheme, int num, SchemeStack::iterator stack);
SchemeObject* s_string_length(Scheme* scheme, SchemeObject* s);
SchemeObject* s_string_ref(Scheme* scheme, SchemeObject* s, SchemeObject* i);
SchemeObject* s_string_set_e(Scheme* scheme, SchemeObject* str, SchemeObject* i, SchemeObject* chr);
SchemeObject* s_symbol_2_string(Scheme* scheme, SchemeObject* symbol);
SchemeObject* s_string_2_symbol(Scheme* scheme, SchemeObject* s);
SchemeObject* s_number_2_string(Scheme* scheme, SchemeObject* n, SchemeObject* base);
SchemeObject* s_string_2_number(Scheme* scheme, SchemeObject* s, SchemeObject* base);
SchemeObject* s_string_append(Scheme* scheme, int num, SchemeStack::iterator stack);
SchemeObject* s_string_copy(Scheme* scheme, SchemeObject* string);
SchemeObject* s_string_2_list(Scheme* scheme, SchemeObject* s);
SchemeObject* s_list_2_string(Scheme* scheme, SchemeObject* p);
SchemeObject* s_substring(Scheme* scheme, SchemeObject* str, SchemeObject* start, SchemeObject* end);
SchemeObject* s_string_equal_p(Scheme* scheme, int num, SchemeStack::iterator args);
SchemeObject* s_string_less_p(Scheme* scheme, int num, SchemeStack::iterator args);
SchemeObject* s_string_greater_p(Scheme* scheme, int num, SchemeStack::iterator args);
SchemeObject* s_string_less_equal_p(Scheme* scheme, int num, SchemeStack::iterator args);
SchemeObject* s_string_greater_equal_p(Scheme* scheme, int num, SchemeStack::iterator args);
SchemeObject* s_string_ci_equal_p(Scheme* scheme, int num, SchemeStack::iterator args);
SchemeObject* s_string_ci_less_p(Scheme* scheme, int num, SchemeStack::iterator args);
SchemeObject* s_string_ci_greater_p(Scheme* scheme, int num, SchemeStack::iterator args);
SchemeObject* s_string_ci_less_equal_p(Scheme* scheme, int num, SchemeStack::iterator args);
SchemeObject* s_string_ci_greater_equal_p(Scheme* scheme, int num, SchemeStack::iterator args);
SchemeObject* s_symgen(Scheme* scheme);

// Input and output stuff
SchemeObject* s_load(Scheme* scheme, SchemeObject* filename);

// My extensions
SchemeObject* i_find_duplicate(SchemeObject* l);
SchemeObject* i_circular_list_p(SchemeObject* p);

#endif
