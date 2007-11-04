

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "scheme.h"
#include <fstream>
#include <iomanip>
#include <cmath>
#include <complex>
#include <cctype>
#include <stdexcept>
#include <cerrno>

#include "lexer.h"
#include "parser.h"
#include "interpreter.h"
#include "filenames.h"

scheme_exception::scheme_exception(wstring s) : str(s), procname(NULL) {
}

scheme_exception::scheme_exception(wchar_t* procname, wstring error) : str(error), procname(procname) {
}

scheme_exception::scheme_exception(uint32_t line, wstring error) : procname(NULL) {
    std::wostringstream ss;
    ss << L"line " << line << L": " << error;
    str = ss.str();
}

wstring scheme_exception::toString() {
    if (procname != NULL) {
        return L"In procedure " + wstring(procname) + L": " + str;            
    } else {
        return str;    
    }     
}

unsigned long symgen_counter = 10000;

SchemeObject* S_TRUE = SchemeObject::createBool(true);
SchemeObject* S_FALSE = SchemeObject::createBool(false);
SchemeObject* S_EOF = SchemeObject::createEOF();
SchemeObject* S_ZERO =  SchemeObject::createIntegerNumber(0);
SchemeObject* S_ONE =   SchemeObject::createIntegerNumber(1);
SchemeObject* S_TWO =   SchemeObject::createIntegerNumber(2);
SchemeObject* S_THREE = SchemeObject::createIntegerNumber(3);
SchemeObject* S_FOUR =  SchemeObject::createIntegerNumber(4);
SchemeObject* S_FIVE =  SchemeObject::createIntegerNumber(5);
SchemeObject* S_SIX =   SchemeObject::createIntegerNumber(6);
SchemeObject* S_SEVEN = SchemeObject::createIntegerNumber(7);
SchemeObject* S_EIGHT = SchemeObject::createIntegerNumber(8);
SchemeObject* S_NINE =  SchemeObject::createIntegerNumber(9);
SchemeObject* S_UNSPECIFIED = SchemeObject::createUnspecified();
SchemeObject* S_EMPTY_LIST = SchemeObject::createEmptyList();
SchemeObject* S_SPACE = char2scm(' ');
SchemeObject* S_NUMBERS[] = {S_ZERO, S_ONE, S_TWO, S_THREE, S_FOUR, S_FIVE, S_SIX, S_SEVEN, S_EIGHT, S_NINE};

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
SchemeObject* quote_symbol;
SchemeObject* quasiquote_symbol;
SchemeObject* unquote_symbol;
SchemeObject* unquote_splicing_symbol;
SchemeObject* define_symbol;
SchemeObject* define_macro;
SchemeObject* set_e_symbol;
SchemeObject* unnamed_symbol;

SchemeObject* current_input_port = NULL;
SchemeObject* current_output_port = NULL;

Interpreter* interpreter = new Interpreter();

//#define R5RS_STRICT

#ifdef R5RS_STRICT 
//SchemeObject* null_environment = SchemeObject::createEnvironment(NULL);
//SchemeObject* scheme_report_environment = SchemeObject::createEnvironment(null_environment);
//SchemeObject* interaction_environment = SchemeObject::createEnvironment(scheme_report_environment);
#else 
SchemeObject* null_environment = SchemeObject::createEnvironment(NULL,256);
SchemeObject* scheme_report_environment = null_environment;
SchemeObject* interaction_environment = null_environment;
#endif

// Global parser used by s_read()
Parser* global_parser = new Parser();

bool globals_init = false;

Scheme::Scheme() {
    if (!globals_init) {
        globals_init = true;

    	assign(L"map"                   ,2,0,1, (SchemeObject* (*)()) s_map, scheme_report_environment);
    	assign(L"for-each"              ,1,0,1, (SchemeObject* (*)()) s_for_each, scheme_report_environment);
    	assign(L"equal?"                ,2,0,0, (SchemeObject* (*)()) s_equal_p, scheme_report_environment);
    	assign(L"eq?"                   ,2,0,0, (SchemeObject* (*)()) s_eq_p, scheme_report_environment);
    	assign(L"eqv?"                  ,2,0,0, (SchemeObject* (*)()) s_eqv_p, scheme_report_environment);
    	assign(L"boolean?"              ,1,0,0, (SchemeObject* (*)()) s_boolean_p, scheme_report_environment);
    	assign(L"pair?"                 ,1,0,0, (SchemeObject* (*)()) s_pair_p, scheme_report_environment);
    	assign(L"symbol?"               ,1,0,0, (SchemeObject* (*)()) s_symbol_p, scheme_report_environment);
    	assign(L"char?"                 ,1,0,0, (SchemeObject* (*)()) s_char_p, scheme_report_environment);
    	assign(L"list?"                 ,1,0,0, (SchemeObject* (*)()) s_list_p, scheme_report_environment);
    	assign(L"string?"               ,1,0,0, (SchemeObject* (*)()) s_string_p, scheme_report_environment);
    	assign(L"procedure?"            ,1,0,0, (SchemeObject* (*)()) s_procedure_p, scheme_report_environment);
    	assign(L"number?"               ,1,0,0, (SchemeObject* (*)()) s_number_p, scheme_report_environment);
    	assign(L"integer?"              ,1,0,0, (SchemeObject* (*)()) s_integer_p, scheme_report_environment);
    	assign(L"complex?"              ,1,0,0, (SchemeObject* (*)()) s_complex_p, scheme_report_environment);
    	assign(L"real?"                 ,1,0,0, (SchemeObject* (*)()) s_real_p, scheme_report_environment);
    	assign(L"rational?"             ,1,0,0, (SchemeObject* (*)()) s_rational_p, scheme_report_environment);
    	assign(L"exact?"                ,1,0,0, (SchemeObject* (*)()) s_exact_p, scheme_report_environment);
    	assign(L"inexact?"              ,1,0,0, (SchemeObject* (*)()) s_inexact_p, scheme_report_environment);
    	assign(L"vector?"               ,1,0,0, (SchemeObject* (*)()) s_vector_p, scheme_report_environment);
    	assign(L"null?"                 ,1,0,0, (SchemeObject* (*)()) s_null_p, scheme_report_environment);
    	assign(L"car"                   ,1,0,0, (SchemeObject* (*)()) s_car, scheme_report_environment);
    	assign(L"cdr"                   ,1,0,0, (SchemeObject* (*)()) s_cdr, scheme_report_environment);
    	assign(L"caar"                  ,1,0,0, (SchemeObject* (*)()) s_caar, scheme_report_environment);
    	assign(L"cadr"                  ,1,0,0, (SchemeObject* (*)()) s_cadr, scheme_report_environment);
    	assign(L"cdar"                  ,1,0,0, (SchemeObject* (*)()) s_cdar, scheme_report_environment);
    	assign(L"cddr"                  ,1,0,0, (SchemeObject* (*)()) s_cddr, scheme_report_environment);
    	assign(L"caaar"                 ,1,0,0, (SchemeObject* (*)()) s_caaar, scheme_report_environment);
    	assign(L"caadr"                 ,1,0,0, (SchemeObject* (*)()) s_caadr, scheme_report_environment);
    	assign(L"cadar"                 ,1,0,0, (SchemeObject* (*)()) s_cadar, scheme_report_environment);
    	assign(L"caddr"                 ,1,0,0, (SchemeObject* (*)()) s_caddr, scheme_report_environment);
    	assign(L"cdaar"                 ,1,0,0, (SchemeObject* (*)()) s_cdaar, scheme_report_environment);
    	assign(L"cdadr"                 ,1,0,0, (SchemeObject* (*)()) s_cdadr, scheme_report_environment);
    	assign(L"cddar"                 ,1,0,0, (SchemeObject* (*)()) s_cddar, scheme_report_environment);
    	assign(L"cdddr"                 ,1,0,0, (SchemeObject* (*)()) s_cdddr, scheme_report_environment);
    	assign(L"caaaar"                ,1,0,0, (SchemeObject* (*)()) s_caaaar, scheme_report_environment);
    	assign(L"caaadr"                ,1,0,0, (SchemeObject* (*)()) s_caaadr, scheme_report_environment);
    	assign(L"caadar"                ,1,0,0, (SchemeObject* (*)()) s_caadar, scheme_report_environment);
    	assign(L"caaddr"                ,1,0,0, (SchemeObject* (*)()) s_caaddr, scheme_report_environment);
    	assign(L"cadaar"                ,1,0,0, (SchemeObject* (*)()) s_cadaar, scheme_report_environment);
    	assign(L"cadadr"                ,1,0,0, (SchemeObject* (*)()) s_cadadr, scheme_report_environment);
    	assign(L"caddar"                ,1,0,0, (SchemeObject* (*)()) s_caddar, scheme_report_environment);
    	assign(L"cadddr"                ,1,0,0, (SchemeObject* (*)()) s_cadddr, scheme_report_environment);                                       
    	assign(L"cdaaar"                ,1,0,0, (SchemeObject* (*)()) s_cdaaar, scheme_report_environment);
    	assign(L"cdaadr"                ,1,0,0, (SchemeObject* (*)()) s_cdaadr, scheme_report_environment);
    	assign(L"cdadar"                ,1,0,0, (SchemeObject* (*)()) s_cdadar, scheme_report_environment);
    	assign(L"cdaddr"                ,1,0,0, (SchemeObject* (*)()) s_cdaddr, scheme_report_environment);
    	assign(L"cddaar"                ,1,0,0, (SchemeObject* (*)()) s_cddaar, scheme_report_environment);
    	assign(L"cddadr"                ,1,0,0, (SchemeObject* (*)()) s_cddadr, scheme_report_environment);
    	assign(L"cdddar"                ,1,0,0, (SchemeObject* (*)()) s_cdddar, scheme_report_environment);
    	assign(L"cddddr"                ,1,0,0, (SchemeObject* (*)()) s_cddddr, scheme_report_environment);
    	assign(L"list"                  ,0,0,1, (SchemeObject* (*)()) s_list, scheme_report_environment);
    	assign(L"list-tail"             ,2,0,0, (SchemeObject* (*)()) s_list_tail, scheme_report_environment);
    	assign(L"list-ref"              ,2,0,0, (SchemeObject* (*)()) s_list_ref, scheme_report_environment);
    	assign(L"set-car!"              ,2,0,0, (SchemeObject* (*)()) s_set_car_e, scheme_report_environment);
    	assign(L"set-cdr!"              ,2,0,0, (SchemeObject* (*)()) s_set_cdr_e, scheme_report_environment);
    	assign(L"assoc"                 ,2,0,0, (SchemeObject* (*)()) s_assoc, scheme_report_environment);
    	assign(L"assq"                  ,2,0,0, (SchemeObject* (*)()) s_assq, scheme_report_environment);
    	assign(L"assv"                  ,2,0,0, (SchemeObject* (*)()) s_assv, scheme_report_environment);
    	assign(L"append"                ,0,0,1, (SchemeObject* (*)()) s_append, scheme_report_environment);
    	assign(L"member"                ,2,0,0, (SchemeObject* (*)()) s_member, scheme_report_environment);
    	assign(L"memq"                  ,2,0,0, (SchemeObject* (*)()) s_memq, scheme_report_environment);
    	assign(L"memv"                  ,2,0,0, (SchemeObject* (*)()) s_memv, scheme_report_environment);
    	assign(L"reverse"               ,1,0,0, (SchemeObject* (*)()) s_reverse, scheme_report_environment);
    	assign(L"length"                ,1,0,0, (SchemeObject* (*)()) s_length, scheme_report_environment);
    	assign(L"cons"                  ,2,0,0, (SchemeObject* (*)()) s_cons, scheme_report_environment);
    	assign(L"display"               ,1,1,0, (SchemeObject* (*)()) s_display, scheme_report_environment);
    	assign(L"write"                 ,1,1,0, (SchemeObject* (*)()) s_write, scheme_report_environment);
    	assign(L"newline"               ,0,1,0, (SchemeObject* (*)()) s_newline, scheme_report_environment);
    	assign(L"<"                     ,0,0,1, (SchemeObject* (*)()) s_less, scheme_report_environment);
    	assign(L">"                     ,0,0,1, (SchemeObject* (*)()) s_greater, scheme_report_environment);
    	assign(L"<="                    ,0,0,1, (SchemeObject* (*)()) s_less_equal, scheme_report_environment);
    	assign(L">="                    ,0,0,1, (SchemeObject* (*)()) s_greater_equal, scheme_report_environment);
    	assign(L"="                     ,0,0,1, (SchemeObject* (*)()) s_equal, scheme_report_environment);
    	assign(L"+"                     ,0,0,1, (SchemeObject* (*)()) s_plus, scheme_report_environment);
    	assign(L"-"                     ,1,0,1, (SchemeObject* (*)()) s_minus, scheme_report_environment);
    	assign(L"*"                     ,0,0,1, (SchemeObject* (*)()) s_mult, scheme_report_environment);
    	assign(L"/"                     ,1,0,1, (SchemeObject* (*)()) s_divide, scheme_report_environment);
    	assign(L"abs"                   ,1,0,0, (SchemeObject* (*)()) s_abs, scheme_report_environment);
    	assign(L"tan"                   ,1,0,0, (SchemeObject* (*)()) s_tan, scheme_report_environment);
    	assign(L"atan"                  ,1,1,0, (SchemeObject* (*)()) s_atan, scheme_report_environment);
    	assign(L"sin"                   ,1,0,0, (SchemeObject* (*)()) s_sin, scheme_report_environment);
    	assign(L"asin"                  ,1,0,0, (SchemeObject* (*)()) s_asin, scheme_report_environment);
    	assign(L"cos"                   ,1,0,0, (SchemeObject* (*)()) s_cos, scheme_report_environment);
    	assign(L"acos"                  ,1,0,0, (SchemeObject* (*)()) s_acos, scheme_report_environment);
    	assign(L"sqrt"                  ,1,0,0, (SchemeObject* (*)()) s_sqrt, scheme_report_environment);
    	assign(L"log"                   ,1,0,0, (SchemeObject* (*)()) s_log, scheme_report_environment);
    	assign(L"exp"                   ,1,0,0, (SchemeObject* (*)()) s_exp, scheme_report_environment);
    	assign(L"expt"                  ,2,0,0, (SchemeObject* (*)()) s_expt, scheme_report_environment);
    	assign(L"round"                 ,1,0,0, (SchemeObject* (*)()) s_round, scheme_report_environment);
    	assign(L"ceiling"               ,1,0,0, (SchemeObject* (*)()) s_ceiling, scheme_report_environment);
    	assign(L"floor"                 ,1,0,0, (SchemeObject* (*)()) s_floor, scheme_report_environment);
    	assign(L"truncate"              ,1,0,0, (SchemeObject* (*)()) s_truncate, scheme_report_environment);
    	assign(L"quotient"              ,2,0,0, (SchemeObject* (*)()) s_quotient, scheme_report_environment);
    	assign(L"remainder"             ,2,0,0, (SchemeObject* (*)()) s_remainder, scheme_report_environment);
    	assign(L"modulo"                ,2,0,0, (SchemeObject* (*)()) s_modulo, scheme_report_environment);
    	assign(L"min"                   ,1,0,1, (SchemeObject* (*)()) s_min, scheme_report_environment);
    	assign(L"max"                   ,1,0,1, (SchemeObject* (*)()) s_max, scheme_report_environment);
    	assign(L"gcd"                   ,0,0,1, (SchemeObject* (*)()) s_gcd, scheme_report_environment);
    	assign(L"lcm"                   ,0,0,1, (SchemeObject* (*)()) s_lcm, scheme_report_environment);
    	assign(L"numerator"             ,1,0,0, (SchemeObject* (*)()) s_numerator, scheme_report_environment);
    	assign(L"denominator"           ,1,0,0, (SchemeObject* (*)()) s_denominator, scheme_report_environment);
    	assign(L"exact->inexact"        ,1,0,0, (SchemeObject* (*)()) s_exact_2_inexact, scheme_report_environment);
    	assign(L"inexact->exact"        ,1,0,0, (SchemeObject* (*)()) s_inexact_2_exact, scheme_report_environment);
    	assign(L"even?"                 ,1,0,0, (SchemeObject* (*)()) s_even_p, scheme_report_environment);
    	assign(L"odd?"                  ,1,0,0, (SchemeObject* (*)()) s_odd_p, scheme_report_environment);
    	assign(L"zero?"                 ,1,0,0, (SchemeObject* (*)()) s_zero_p, scheme_report_environment);
    	assign(L"negative?"             ,1,0,0, (SchemeObject* (*)()) s_negative_p, scheme_report_environment);
    	assign(L"positive?"             ,1,0,0, (SchemeObject* (*)()) s_positive_p, scheme_report_environment);
    	assign(L"make-rectangular"      ,2,0,0, (SchemeObject* (*)()) s_make_rectangular, scheme_report_environment);
    	assign(L"make-polar"            ,2,0,0, (SchemeObject* (*)()) s_make_polar, scheme_report_environment);
    	assign(L"real-part"             ,1,0,0, (SchemeObject* (*)()) s_real_part, scheme_report_environment);
    	assign(L"imag-part"             ,1,0,0, (SchemeObject* (*)()) s_imag_part, scheme_report_environment);
    	assign(L"magnitude"             ,1,0,0, (SchemeObject* (*)()) s_magnitude, scheme_report_environment);
    	assign(L"angle"                 ,1,0,0, (SchemeObject* (*)()) s_angle, scheme_report_environment);
    	
    	assign(L"not"                   ,1,0,0, (SchemeObject* (*)()) s_not, scheme_report_environment);
    	assign(L"make-vector"           ,1,1,0, (SchemeObject* (*)()) s_make_vector, scheme_report_environment);
    	assign(L"vector"                ,0,0,1, (SchemeObject* (*)()) s_vector, scheme_report_environment);
    	assign(L"vector-length"         ,1,0,0, (SchemeObject* (*)()) s_vector_length, scheme_report_environment);
    	assign(L"vector-ref"            ,2,0,0, (SchemeObject* (*)()) s_vector_ref, scheme_report_environment);
    	assign(L"vector-set!"           ,3,0,0, (SchemeObject* (*)()) s_vector_set_e, scheme_report_environment);
    	assign(L"vector-fill!"          ,2,0,0, (SchemeObject* (*)()) s_vector_fill_e, scheme_report_environment);
    	assign(L"list->vector"          ,1,0,0, (SchemeObject* (*)()) s_list_2_vector, scheme_report_environment);
    	assign(L"vector->list"          ,1,0,0, (SchemeObject* (*)()) s_vector_2_list, scheme_report_environment);

    	assign(L"make-string"           ,1,1,0, (SchemeObject* (*)()) s_make_string, scheme_report_environment);
    	assign(L"string"                ,0,0,1, (SchemeObject* (*)()) s_string, scheme_report_environment);
    	assign(L"string-length"         ,1,0,0, (SchemeObject* (*)()) s_string_length, scheme_report_environment);
    	assign(L"string-ref"            ,2,0,0, (SchemeObject* (*)()) s_string_ref, scheme_report_environment);
    	assign(L"string-set!"           ,3,0,0, (SchemeObject* (*)()) s_string_set_e, scheme_report_environment);
    	assign(L"string-append"         ,0,0,1, (SchemeObject* (*)()) s_string_append, scheme_report_environment);
    	assign(L"string-copy"           ,1,0,0, (SchemeObject* (*)()) s_string_copy, scheme_report_environment);
    	assign(L"substring"             ,3,0,0, (SchemeObject* (*)()) s_substring, scheme_report_environment);
    	assign(L"symbol->string"        ,1,0,0, (SchemeObject* (*)()) s_symbol_2_string, scheme_report_environment);
    	assign(L"string->symbol"        ,1,0,0, (SchemeObject* (*)()) s_string_2_symbol, scheme_report_environment);
    	assign(L"char->integer"         ,1,0,0, (SchemeObject* (*)()) s_char_2_integer, scheme_report_environment);
    	assign(L"integer->char"         ,1,0,0, (SchemeObject* (*)()) s_integer_2_char, scheme_report_environment);
    	assign(L"number->string"        ,1,1,0, (SchemeObject* (*)()) s_number_2_string, scheme_report_environment);
    	assign(L"string->number"        ,1,1,0, (SchemeObject* (*)()) s_string_2_number, scheme_report_environment);
    	assign(L"list->string"          ,1,0,0, (SchemeObject* (*)()) s_list_2_string, scheme_report_environment);
    	assign(L"string->list"          ,1,0,0, (SchemeObject* (*)()) s_string_2_list, scheme_report_environment);
    	assign(L"string=?"              ,0,0,1, (SchemeObject* (*)()) s_string_equal_p, scheme_report_environment);
    	assign(L"string<?"              ,0,0,1, (SchemeObject* (*)()) s_string_less_p, scheme_report_environment);
    	assign(L"string>?"              ,0,0,1, (SchemeObject* (*)()) s_string_greater_p, scheme_report_environment);
    	assign(L"string<=?"             ,0,0,1, (SchemeObject* (*)()) s_string_less_equal_p, scheme_report_environment);
    	assign(L"string>=?"             ,0,0,1, (SchemeObject* (*)()) s_string_greater_equal_p, scheme_report_environment);
    	assign(L"string-ci=?"           ,0,0,1, (SchemeObject* (*)()) s_string_ci_equal_p, scheme_report_environment);
    	assign(L"string-ci<?"           ,0,0,1, (SchemeObject* (*)()) s_string_ci_less_p, scheme_report_environment);
    	assign(L"string-ci>?"           ,0,0,1, (SchemeObject* (*)()) s_string_ci_greater_p, scheme_report_environment);
    	assign(L"string-ci<=?"          ,0,0,1, (SchemeObject* (*)()) s_string_ci_less_equal_p, scheme_report_environment);
    	assign(L"string-ci>=?"          ,0,0,1, (SchemeObject* (*)()) s_string_ci_greater_equal_p, scheme_report_environment);

    	assign(L"char-downcase"         ,1,0,0, (SchemeObject* (*)()) s_char_downcase, scheme_report_environment);
    	assign(L"char-upcase"           ,1,0,0, (SchemeObject* (*)()) s_char_upcase, scheme_report_environment);
    	assign(L"char-alphabetic?"      ,1,0,0, (SchemeObject* (*)()) s_char_alphabetic_p, scheme_report_environment);
    	assign(L"char-numeric?"         ,1,0,0, (SchemeObject* (*)()) s_char_numeric_p, scheme_report_environment);
    	assign(L"char-whitespace?"      ,1,0,0, (SchemeObject* (*)()) s_char_whitespace_p, scheme_report_environment);
    	assign(L"char-upper-case?"      ,1,0,0, (SchemeObject* (*)()) s_char_upper_case_p, scheme_report_environment);
    	assign(L"char-lower-case?"      ,1,0,0, (SchemeObject* (*)()) s_char_lower_case_p, scheme_report_environment);
    	assign(L"char=?"                ,0,0,1, (SchemeObject* (*)()) s_char_equal_p, scheme_report_environment);
    	assign(L"char<?"                ,0,0,1, (SchemeObject* (*)()) s_char_less_p, scheme_report_environment);
    	assign(L"char>?"                ,0,0,1, (SchemeObject* (*)()) s_char_greater_p, scheme_report_environment);
    	assign(L"char<=?"               ,0,0,1, (SchemeObject* (*)()) s_char_less_equal_p, scheme_report_environment);
    	assign(L"char>=?"               ,0,0,1, (SchemeObject* (*)()) s_char_greater_equal_p, scheme_report_environment);
    	assign(L"char-ci=?"             ,0,0,1, (SchemeObject* (*)()) s_char_ci_equal_p, scheme_report_environment);
    	assign(L"char-ci<?"             ,0,0,1, (SchemeObject* (*)()) s_char_ci_less_p, scheme_report_environment);
    	assign(L"char-ci>?"             ,0,0,1, (SchemeObject* (*)()) s_char_ci_greater_p, scheme_report_environment);
    	assign(L"char-ci<=?"            ,0,0,1, (SchemeObject* (*)()) s_char_ci_less_equal_p, scheme_report_environment);
    	assign(L"char-ci>=?"            ,0,0,1, (SchemeObject* (*)()) s_char_ci_greater_equal_p, scheme_report_environment);
    	assign(L"symgen"                ,0,0,0, (SchemeObject* (*)()) s_symgen, scheme_report_environment);

    	assign(L"current-input-port"    ,0,0,0, (SchemeObject* (*)()) s_current_input_port, scheme_report_environment);
    	assign(L"current-output-port"   ,0,0,0, (SchemeObject* (*)()) s_current_output_port, scheme_report_environment);
    	assign(L"input-port?"           ,1,0,0, (SchemeObject* (*)()) s_input_port_p, scheme_report_environment);
    	assign(L"output-port?"          ,1,0,0, (SchemeObject* (*)()) s_output_port_p, scheme_report_environment);
    	assign(L"eof-object?"           ,1,0,0, (SchemeObject* (*)()) s_eof_object_p, scheme_report_environment);
    	assign(L"call-with-input-file"  ,2,0,0, (SchemeObject* (*)()) s_call_with_input_file, scheme_report_environment);
    	assign(L"call-with-output-file" ,2,0,0, (SchemeObject* (*)()) s_call_with_output_file, scheme_report_environment);
    	assign(L"with-input-from-file"  ,2,0,0, (SchemeObject* (*)()) s_with_input_from_file, scheme_report_environment);
    	assign(L"with-output-to-file"   ,2,0,0, (SchemeObject* (*)()) s_with_output_to_file, scheme_report_environment);
    	assign(L"open-input-file"       ,1,0,0, (SchemeObject* (*)()) s_open_input_file, scheme_report_environment);
    	assign(L"open-output-file"      ,1,0,0, (SchemeObject* (*)()) s_open_output_file, scheme_report_environment);
    	assign(L"close-input-port"      ,1,0,0, (SchemeObject* (*)()) s_close_input_port, scheme_report_environment);
    	assign(L"close-output-port"     ,1,0,0, (SchemeObject* (*)()) s_close_output_port, scheme_report_environment);
    	assign(L"read-char"             ,0,1,0, (SchemeObject* (*)()) s_read_char, scheme_report_environment);
    	assign(L"peek-char"             ,0,1,0, (SchemeObject* (*)()) s_peek_char, scheme_report_environment);
    	assign(L"write-char"            ,1,1,0, (SchemeObject* (*)()) s_write_char, scheme_report_environment);
    	assign(L"read"                  ,0,1,0, (SchemeObject* (*)()) s_read, scheme_report_environment);
    	assign(L"load"                  ,1,0,0, (SchemeObject* (*)()) s_load, scheme_report_environment);

    	assign(L"apply"                 ,1,0,1, (SchemeObject* (*)()) s_apply, scheme_report_environment);
    	assign(L"call-with-current-continuation" ,1,0,0, (SchemeObject* (*)()) s_call_cc, scheme_report_environment);
    	assign(L"call/cc"               ,1,0,0, (SchemeObject* (*)()) s_call_cc, scheme_report_environment);
        assign(L"eval"                  ,2,0,0, (SchemeObject* (*)()) s_eval, scheme_report_environment);
        assign(L"scheme-report-environment",1,0,0, (SchemeObject* (*)()) s_scheme_report_environment, scheme_report_environment);
        assign(L"null-environment"      ,1,0,0, (SchemeObject* (*)()) s_null_environment, scheme_report_environment);
        assign(L"interaction-environment",1,0,0, (SchemeObject* (*)()) s_interaction_environment, scheme_report_environment);


        //assign("apply", new SchemeInternalProcedure("apply"));
        assign(L"if",           SchemeObject::createInternalProcedure(L"if"), null_environment);
        assign(L"cond",         SchemeObject::createInternalProcedure(L"cond"), null_environment);
        assign(L"case",         SchemeObject::createInternalProcedure(L"case"), null_environment);
        assign(L"do",           SchemeObject::createInternalProcedure(L"do"), null_environment);
        assign(L"let",          SchemeObject::createInternalProcedure(L"let"), null_environment);
        assign(L"let*",         SchemeObject::createInternalProcedure(L"let*"), null_environment);
        assign(L"letrec",       SchemeObject::createInternalProcedure(L"letrec"), null_environment);
        assign(L"begin",        SchemeObject::createInternalProcedure(L"begin"), null_environment);
        assign(L"and",          SchemeObject::createInternalProcedure(L"and"), null_environment);
        assign(L"or",           SchemeObject::createInternalProcedure(L"or"), null_environment);
        assign(L"lambda",       SchemeObject::createInternalProcedure(L"lambda"), null_environment);
        assign(L"quote",        SchemeObject::createInternalProcedure(L"quote"), null_environment);
        assign(L"quasiquote",   SchemeObject::createInternalProcedure(L"quasiquote"), null_environment);
        assign(L"define",       SchemeObject::createInternalProcedure(L"define"), null_environment);
        assign(L"define-macro", SchemeObject::createInternalProcedure(L"define-macro"), null_environment);
        assign(L"set!",         SchemeObject::createInternalProcedure(L"set!"), null_environment);

        current_input_port = SchemeObject::createInputPort(&wcin);
        current_output_port = SchemeObject::createOutputPort(&wcout);

        else_symbol = SchemeObject::createSymbol(L"else");
        ergo_symbol = SchemeObject::createSymbol(L"=>");
        unquote_symbol = SchemeObject::createSymbol(L"unquote");
        unquote_splicing_symbol = SchemeObject::createSymbol(L"unquote-splicing");
        unnamed_symbol = SchemeObject::createSymbol(L"#<unnamed>");

        if_symbol = SchemeObject::createSymbol(L"if");
        apply_symbol = SchemeObject::createSymbol(L"apply");
        cond_symbol = SchemeObject::createSymbol(L"cond");
        case_symbol = SchemeObject::createSymbol(L"case");
        do_symbol = SchemeObject::createSymbol(L"do");
        let_symbol = SchemeObject::createSymbol(L"let");
        letstar_symbol = SchemeObject::createSymbol(L"let*");
        letrec_symbol = SchemeObject::createSymbol(L"letrec");
        begin_symbol = SchemeObject::createSymbol(L"begin");
        and_symbol = SchemeObject::createSymbol(L"and");
        or_symbol = SchemeObject::createSymbol(L"or");
        lambda_symbol = SchemeObject::createSymbol(L"lambda");
        quote_symbol = SchemeObject::createSymbol(L"quote");
        quasiquote_symbol = SchemeObject::createSymbol(L"quasiquote");
        define_symbol = SchemeObject::createSymbol(L"define");
        define_macro = SchemeObject::createSymbol(L"define-macro");
        set_e_symbol = SchemeObject::createSymbol(L"set!");
        Heap* heap = Heap::getUniqueInstance();

        heap->addRoot(current_output_port);
        heap->addRoot(current_input_port);
        heap->addRoot(interaction_environment);
        heap->addRoot(S_SPACE);
        heap->addRoot(S_UNSPECIFIED);
        heap->addRoot(S_EMPTY_LIST);
        heap->addRoot(S_EOF);
        heap->addRoot(S_TRUE);
        heap->addRoot(S_FALSE);
        heap->addRoot(S_ZERO);
        heap->addRoot(S_ONE);
        heap->addRoot(S_TWO);
        heap->addRoot(S_THREE);
        heap->addRoot(S_FOUR);
        heap->addRoot(S_FIVE);
        heap->addRoot(S_SIX);
        heap->addRoot(S_SEVEN);
        heap->addRoot(S_EIGHT);
        heap->addRoot(S_NINE);

        heap->addRoot(else_symbol);
        heap->addRoot(ergo_symbol);
        heap->addRoot(unquote_symbol);
        heap->addRoot(unquote_splicing_symbol);
        heap->addRoot(unnamed_symbol);
        
        eval(L"(define-macro (values . x) `(list ,@x))", scheme_report_environment);
        eval(L"(define-macro (call-with-values f g)  `(apply ,g (,f)))", scheme_report_environment);

    }

    /*
    wifstream infile;
    infile.open("init.scm", ifstream::in);
    eval(&infile, scheme_report_environment);
    infile.close();
    */
}

SchemeObject* Scheme::eval(wistream* is, SchemeObject* envt) {
    if (envt == NULL) envt = interaction_environment;        
    SchemeObject* parse_tree = global_parser->parse(is);
    return interpreter->interpret(parse_tree, envt);
}

SchemeObject* Scheme::eval(wstring data, SchemeObject* envt) {
    wistream* is = new wistringstream(data);
    SchemeObject* result = eval(is);
    delete is;
    return result;
};

void Scheme::assign(wstring variable, int req, int opt, int rst, SchemeObject* (*fn)(), SchemeObject* envt) {
    if (envt == NULL) envt = interaction_environment;        
    SchemeObject* name = SchemeObject::createSymbol(variable.c_str());
    SchemeObject* proc = SchemeObject::createBuiltinProcedure(name, req, opt, rst, fn);
    envt->defineBinding(name, proc);
}

void Scheme::assign(wstring variable, SchemeObject* value, SchemeObject* envt) {
    if (envt == NULL) envt = interaction_environment;        
    SchemeObject* name = SchemeObject::createSymbol(variable.c_str());
    envt->defineBinding(name, value);
}

void Scheme::assign(wstring variable, double value, SchemeObject* envt) {
    assign(variable,double2scm(value),envt);        
}

SchemeObject* Scheme::lookup(SchemeObject* symbol, SchemeObject* envt) {
    if (envt == NULL) envt = interaction_environment;        
    return envt->getBinding(symbol);    
}

SchemeObject* Scheme::lookupOrFail(SchemeObject* symbol, SchemeObject* envt) {
    if (envt == NULL) envt = interaction_environment;        
    SchemeObject* result = envt->getBinding(symbol);    
    if (result == NULL) {
        throw scheme_exception(L"Unbound symbol: " + symbol->toString());    
    }
    return result;
}

SchemeObject* Scheme::lookup(wstring variable, SchemeObject* envt) {
    if (envt == NULL) envt = interaction_environment;        
    SchemeObject* symbol = SchemeObject::createSymbol(variable.c_str());
    return lookup(symbol, envt);
}

void Scheme::forceGarbageCollection() {
    Heap::getUniqueInstance()->garbageCollect(stack);
}


// -----------------------------------------------------
// Procedures
// -----------------------------------------------------
// Find a duplicate in a list. The list can be improper.
// My extension.
SchemeObject* s_find_duplicate(SchemeObject* l) {
    while(i_pair_p(l) == S_TRUE) {
        SchemeObject* cur = i_car(l);
        SchemeObject* k = s_cdr(l);
        while(i_pair_p(k) == S_TRUE) {
            if (i_car(k) == cur) {
                return cur;            
            }
            k = i_cdr(k);
            if (i_pair_p(k) == S_FALSE) {
                if (k == cur) {
                    return cur;            
                }    
            }
        }
        l = i_cdr(l);
    }
    return S_FALSE;
}

// (equal? a b)
// Equal? recursively compares the contents of pairs, vectors, and strings, applying eqv? on other objects 
// such as numbers and symbols. A rule of thumb is that objects are generally equal? if they print the same. 
// Equal? may fail to terminate if its arguments are circular data structures.
SchemeObject* s_equal_p(SchemeObject* a, SchemeObject* b) {
    bool result;        
    SchemeObject::ObjectType ta = a->type();        
    SchemeObject::ObjectType tb = b->type();
    if (ta == SchemeObject::REAL_NUMBER && tb == SchemeObject::REAL_NUMBER) {
        result = a->realValue() == b->realValue();    
    } else if (ta == SchemeObject::COMPLEX_NUMBER && tb == SchemeObject::COMPLEX_NUMBER) {
        result = a->realValue() == b->realValue();    
    } else if (ta == SchemeObject::INTEGER_NUMBER && tb == SchemeObject::INTEGER_NUMBER) {
        result = a->realValue() == b->realValue();    
    } else {
        result = a->toString() == b->toString();
    }
    return bool2scm(result);
}

SchemeObject* s_eqv_p(SchemeObject* a, SchemeObject* b) {
    SchemeObject::ObjectType ta = a->type();        
    SchemeObject::ObjectType tb = b->type();
    if (ta == SchemeObject::INTEGER_NUMBER && tb == SchemeObject::INTEGER_NUMBER) {
        return bool2scm(scm2int(a) == scm2int(b));
    } else if (ta == SchemeObject::REAL_NUMBER && tb == SchemeObject::REAL_NUMBER) {
        return bool2scm(scm2double(a) == scm2double(b));
    } else if (ta == SchemeObject::RATIONAL_NUMBER && tb == SchemeObject::RATIONAL_NUMBER) {
        return bool2scm(scm2rational(a) == scm2rational(b));
    } else if (ta == SchemeObject::COMPLEX_NUMBER && tb == SchemeObject::COMPLEX_NUMBER) {
        return bool2scm(scm2complex(a) == scm2complex(b));
    } else if (ta == SchemeObject::CHAR && tb == SchemeObject::CHAR) {
        return bool2scm(scm2char(a) == scm2char(b));
    } else {
        return bool2scm(a == b);
    }
}

SchemeObject* s_eq_p(SchemeObject* a, SchemeObject* b) {
    return s_eqv_p(a,b); 
}

// (char? b)
SchemeObject* s_char_p(SchemeObject* o) {
    return o->type() == SchemeObject::CHAR ? S_TRUE : S_FALSE;
}


// (boolean? b)
SchemeObject* s_boolean_p(SchemeObject* o) {
    return (o == S_TRUE || o == S_FALSE) ? S_TRUE : S_FALSE;
}

// (list? a)
SchemeObject* s_list_p(SchemeObject* o) {
    SchemeObject *fast, *slow;
    fast = slow = o;
    while (true) {
        if (i_pair_p(fast) == S_FALSE) return fast == S_EMPTY_LIST ? S_TRUE : S_FALSE;
        fast = i_cdr(fast);
        if (slow == fast) {
            // The fast stepping pointer has looped around and caught up with the slow
            // moving pointer, thus the structure is circular and thus not a list.
            return S_FALSE;
        }
        if (i_pair_p(fast) == S_FALSE) return fast == S_EMPTY_LIST ? S_TRUE : S_FALSE;
        fast = i_cdr(fast);
        slow = i_cdr(slow);
        if (slow == fast) {
            // The fast stepping pointer has looped around and caught up with the slow
            // moving pointer, thus the structure is circular and thus not a list.
            return S_FALSE;
        }
    }
}

SchemeObject* s_circular_list_p(SchemeObject* o) {
    SchemeObject *fast, *slow;
    fast = slow = o;
    while (true) {
        if (s_pair_p(fast) == S_FALSE) return S_FALSE;
        if (s_pair_p(fast) == S_TRUE && s_cdr(fast) == S_EMPTY_LIST) return S_FALSE;
        fast = s_cdr(fast);
        if (slow == fast) {
            // The fast stepping pointer has looped around and caught up with the slow
            // moving pointer, thus the structure is circular and thus not a list.
            return S_TRUE;
        }
        if (s_pair_p(fast) == S_FALSE) return S_FALSE;
        if (s_pair_p(fast) == S_TRUE && s_cdr(fast) == S_EMPTY_LIST) return S_FALSE;
        fast = s_cdr(fast);
        slow = s_cdr(slow);
        if (slow == fast) {
            // The fast stepping pointer has looped around and caught up with the slow
            // moving pointer, thus the structure is circular and thus not a list.
            return S_TRUE;
        }
    }    
}

SchemeObject* Scheme::callProcedure_1(SchemeObject* s_proc, SchemeObject* s_arg1) {
   return interpreter->call_procedure_1(s_proc, s_arg1);        
}

SchemeObject* Scheme::callProcedure_2(SchemeObject* s_proc, SchemeObject* s_arg1, SchemeObject* s_arg2) {
   return interpreter->call_procedure_2(s_proc, s_arg1, s_arg2);        
}

SchemeObject* Scheme::callProcedure_3(SchemeObject* s_proc, SchemeObject* s_arg1, SchemeObject* s_arg2, SchemeObject* s_arg3) {
   return interpreter->call_procedure_3(s_proc, s_arg1, s_arg2, s_arg3);        
}

// TODO: Gør denne til en intern function med tail-optimization
SchemeObject* s_call_cc(SchemeObject* s_proc) {
    assert_arg_type(L"proc", 1, s_procedure_p, s_proc);
    SchemeObject* s_escape = SchemeObject::createContinuation();
    size_t stack_size = stack.size();
    stack.push_back(s_escape);
    s_escape->jmpbuf = (::jmp_buf *)::malloc(sizeof(::jmp_buf));
    int kk = setjmp(*(s_escape->jmpbuf));
    if (kk == 0) {
        SchemeObject* result = interpreter->call_procedure_1(s_proc, s_escape);
        // s_proc didn't call the escape-continuation
        stack.resize(stack_size);
        return result;
    }
    stack.resize(stack_size);
    return s_escape->result;
}

// TODO: Gør denne til en intern function (igen) med tail-optimization i kaldet til proc.
// TODO: Det er måske nemmere at skrive denne som en macro, ala (define-macro (apply proc args) `(,proc ,@args))
// args is a list (arg1 arg2 ... argn). argn must be a list. proc is called with the arguments
// (append (list arg1 arg2 ...) argn)
SchemeObject* s_apply(int num, SchemeStack::iterator args) {
        
    SchemeObject* proc = *args++;
    num--;
    assert_arg_type(L"apply", 1, s_procedure_p, proc);

    stack.push_back(S_EMPTY_LIST);
    SchemeObject*& collected = stack.back();
    SchemeObject* prev = NULL;

    for(int i = 0; i < num; i++) {
        SchemeObject* arg = *args++;
        if (i_pair_p(arg) == S_TRUE || arg == S_EMPTY_LIST) {
            if (i == num - 1) {
                // arg is a list and last argument
                if (collected == S_EMPTY_LIST) {
                    collected = arg;
                } else {
                    s_set_cdr_e(prev, arg);
                }
            } else {
                throw scheme_exception(L"Illegal argument");
            }
        } else {
            if (collected == S_EMPTY_LIST) {
                collected = i_cons(arg, S_EMPTY_LIST);
                prev = collected;
            } else {
                SchemeObject* tmp = i_cons(arg,S_EMPTY_LIST);
                i_set_cdr_e(prev, tmp);
                prev = tmp;
            }
        }
    }
    stack.pop_back();
    return interpreter->call_procedure_n(proc,collected);
}

SchemeObject* s_map_internal(wchar_t* procname, int num, SchemeStack::iterator args, bool collect) {

    assert(num > 0);
    SchemeObject* proc = *args;
    assert_arg_type(procname, 1, s_procedure_p, proc);

    SchemeObject* cropped_args[num-1];
    
    for(int i = 1; i < num; i++) {
        assert_non_atom_type(procname, i, args[i]);
        cropped_args[i-1] = args[i];
    }
    
    SchemeObject* result = S_EMPTY_LIST;
    SchemeObject* prev = S_EMPTY_LIST;

    // Vi skralder af lists i hvert gennemløb. Så ((1 2 3)(10 20 30)) bliver til ((2 3)(20 30)) og til sidst ((3)(30))
    while (cropped_args[0] != S_EMPTY_LIST) {
        // Collect args
        SchemeAppendableList collection;
        for(int i = 0; i < num-1; i++) {
            SchemeObject* lists_ptr = cropped_args[i];
            if (lists_ptr == S_EMPTY_LIST) {
                throw scheme_exception(wstring(procname) + L": argument lists not equals length.");
            }
            SchemeObject* arg = s_car(lists_ptr);
            cropped_args[i] = s_cdr(lists_ptr);
            collection.add(arg);
        }
        
        SchemeObject* result_item = interpreter->call_procedure_n(proc, collection.list);
        
        if (collect) {
           if (result == S_EMPTY_LIST) {
               result = i_cons(result_item, S_EMPTY_LIST);
               prev = result;
               stack.push_back(result);
           } else {
               SchemeObject* tmp = i_cons(result_item, S_EMPTY_LIST);
               i_set_cdr_e(prev, tmp);
               prev = tmp;
           }
        }
    }
    
    // Tjek at alle cropped_args nu er tomme, dvs. at argumentlisterne var lige lange
    for(int i = 0; i < num-1; i++) {
        if (cropped_args[i] != S_EMPTY_LIST) {
            throw scheme_exception(wstring(procname) + L": argument lists not equals length.");
        }
    }
    if (collect && result != S_EMPTY_LIST) {
        stack.pop_back();
    }
    return collect ? result : S_UNSPECIFIED;    
}


SchemeObject* s_map(int num, SchemeStack::iterator args) {
    return s_map_internal(L"map", num, args, true);
}

SchemeObject* s_for_each(int num, SchemeStack::iterator args) {
    return s_map_internal(L"for-each",num, args, false);
}

inline
SchemeObject* member_helper(SchemeObject* (comparator)(SchemeObject*,SchemeObject*), SchemeObject* obj, SchemeObject* p) {
    while (i_null_p(p) == S_FALSE) {
        if ((*comparator)(obj, s_car(p)) == S_TRUE) {
            return p;
        } else {
            p = s_cdr(p);
        }
    }
    return S_FALSE;
}

SchemeObject* s_member(SchemeObject* obj, SchemeObject* p) {
    return member_helper(s_equal_p, obj, p);
}

SchemeObject* s_memq(SchemeObject* obj, SchemeObject* p) {
    return member_helper(s_eq_p, obj, p);
}

SchemeObject* s_memv(SchemeObject* obj, SchemeObject* p) {
    return member_helper(s_eqv_p, obj, p);
}

SchemeObject* s_list_tail(SchemeObject* l, SchemeObject* k) {
    assert_arg_positive_int(L"list-tail", 2, k);
    int i = scm2int(k);
    while (i-- > 0) {
        if (l == S_EMPTY_LIST) {
            throw scheme_exception(L"Index out of range: " + k->toString());
        }
        if (s_pair_p(l) == S_FALSE) {
            throw scheme_exception(L"Not a proper list");
        }
        l = s_cdr(l);
    }
    return l;
}

SchemeObject* s_list_ref(SchemeObject* l, SchemeObject* k) {
    SchemeObject* p = s_list_tail(l, k);
    if (p == S_EMPTY_LIST) {
        throw scheme_exception(L"Index out of range: " + k->toString());
    }
    return s_car(p);
}

SchemeObject* assoc_helper(SchemeObject* (comparator)(SchemeObject*,SchemeObject*), SchemeObject* obj, SchemeObject* alist) {
    while (alist != S_EMPTY_LIST) {
        if (s_pair_p(s_car(alist)) == S_FALSE) {
            throw scheme_exception(L"Illegal argument");
        }
        SchemeObject* p = s_car(alist);
        if ((*comparator)(obj, s_car(p)) == S_TRUE) {
            return p;
        }
        alist = s_cdr(alist);
    }
    return S_FALSE;
}

SchemeObject* s_assoc(SchemeObject* obj, SchemeObject* alist) {
    return assoc_helper(s_equal_p, obj, alist); 
}

SchemeObject* s_assq(SchemeObject* obj, SchemeObject* alist) {
    return assoc_helper(s_eq_p, obj, alist); 
}

SchemeObject* s_assv(SchemeObject* obj, SchemeObject* alist) {
    return assoc_helper(s_eqv_p, obj, alist); 
}

// (pair? p)
SchemeObject* s_pair_p(SchemeObject* o) {
    return i_pair_p(o);
}

// (symbol? p)
SchemeObject* s_symbol_p(SchemeObject* o) {
    return i_symbol_p(o);
}

// (string? p)
SchemeObject* s_string_p(SchemeObject* p) {
    return (p->type() == SchemeObject::STRING) ? S_TRUE : S_FALSE;
}

// (procedure? p)
SchemeObject* s_procedure_p(SchemeObject* p) {
    return i_procedure_p(p);
}

// (number? p)
SchemeObject* s_number_p(SchemeObject* p) {
    return i_number_p(p);
}


SchemeObject* s_complex_p(SchemeObject* n) {
    return i_number_p(n);
}

SchemeObject* s_real_p(SchemeObject* n) {
    if (n->type() == SchemeObject::COMPLEX_NUMBER) {
        return n->complexValue().imag() == 0.0 ? S_TRUE : S_FALSE;
    } else {
        return i_number_p(n);
    }
}

SchemeObject* s_rational_p(SchemeObject* n) {
    if (n->type() == SchemeObject::RATIONAL_NUMBER) {
        return S_TRUE;
    } else if (n->type() == SchemeObject::INTEGER_NUMBER) {
        return S_TRUE;    
    } else {
        return S_FALSE;    
    }
}

// (integer? p)
SchemeObject* s_integer_p(SchemeObject* p) {
    double i;
    if (p->type() == SchemeObject::INTEGER_NUMBER) {
        return S_TRUE;
    } else if (p->type() == SchemeObject::RATIONAL_NUMBER) {
        long denom = p->rationalValue().second;     
        return bool2scm(denom == 1 || denom == -1);
    } else if (p->type() == SchemeObject::REAL_NUMBER) {
        return ::modf(scm2double(p),&i) == 0.0 ? S_TRUE : S_FALSE;
    } else if (p->type() == SchemeObject::COMPLEX_NUMBER) {
        std::complex<double> z = p->complexValue();
        return ::modf(z.real(),&i) == 0.0 && z.imag() == 0.0 ? S_TRUE : S_FALSE;
    } else {
        return S_FALSE;    
    }
}

SchemeObject* s_exact_p(SchemeObject* n) {
    assert_arg_type(L"exact?", 1, s_number_p, n);
    return n->type() == SchemeObject::INTEGER_NUMBER ||
           n->type() == SchemeObject::RATIONAL_NUMBER ? S_TRUE : S_FALSE;
}

SchemeObject* s_inexact_p(SchemeObject* n) {
    assert_arg_type(L"inexact?", 1, s_number_p, n);
    return s_exact_p(n) == S_TRUE ? S_FALSE : S_TRUE;
}

SchemeObject* s_exact_2_inexact(SchemeObject* n) {
    assert_arg_type(L"exact->inexact", 1, s_number_p, n);
    if (n->type() == SchemeObject::INTEGER_NUMBER) {
        return double2scm(n->realValue());
    } else if (n->type() == SchemeObject::RATIONAL_NUMBER) {
        return double2scm(n->realValue());
    } else  {
        return n;
    }
}

SchemeObject* s_inexact_2_exact(SchemeObject* n) {
    assert_arg_type(L"inexact->exact", 1, s_real_p, n);
    if (n->type() == SchemeObject::REAL_NUMBER || n->type() == SchemeObject::COMPLEX_NUMBER) {
        // To converter a double to a rational, we multiply
        // it with 10 until it's an integer number.
        double real = n->realValue();
        double tmp;
        long denominator = 1;
        while (::modf(real,&tmp) != 0.0) {
            real *= 10;
            denominator *= 10;
        }
        long numerator = long(real);
        std::pair<long,long> rational(numerator, denominator);
        return rational2scm(rational);
    } else {
        return n;    
    }
}

// (number? p)
SchemeObject* s_vector_p(SchemeObject* p) {
    return (p->type() == SchemeObject::VECTOR) ? S_TRUE : S_FALSE;
}

// (null? p)
SchemeObject* s_null_p(SchemeObject* p) {
    return i_null_p(p);
}

SchemeObject* s_car(SchemeObject* o) {
    assert_arg_pair_type(L"car", 1, o);
    return i_car(o);
}

SchemeObject* s_cdr(SchemeObject* o) {
    assert_arg_pair_type(L"cdr", 1, o);
    return i_cdr(o);
}

SchemeObject* s_cxr(SchemeObject* o, const char* x) {
    while (*x != '\0') {
        if (*x == 'a') {
            o = s_car(o);
        } else {
            o = s_cdr(o);
        }
        x++;
    }
    return o;
}

SchemeObject* s_caar(SchemeObject* o) { return s_cxr(o, "aa"); };
SchemeObject* s_cadr(SchemeObject* o) { return s_cxr(o, "da"); };
SchemeObject* s_cdar(SchemeObject* o) { return s_cxr(o, "ad"); };
SchemeObject* s_cddr(SchemeObject* o) { return s_cxr(o, "dd"); };
SchemeObject* s_caaar(SchemeObject* o) { return s_cxr(o, "aaa"); };
SchemeObject* s_caadr(SchemeObject* o) { return s_cxr(o, "daa"); };
SchemeObject* s_cadar(SchemeObject* o) { return s_cxr(o, "ada"); };
SchemeObject* s_caddr(SchemeObject* o) { return s_cxr(o, "dda"); };
SchemeObject* s_cdaar(SchemeObject* o) { return s_cxr(o, "aad"); };
SchemeObject* s_cdadr(SchemeObject* o) { return s_cxr(o, "dad"); };
SchemeObject* s_cddar(SchemeObject* o) { return s_cxr(o, "add"); };
SchemeObject* s_cdddr(SchemeObject* o) { return s_cxr(o, "ddd"); };
SchemeObject* s_caaaar(SchemeObject* o) { return s_cxr(o, "aaaa"); };
SchemeObject* s_caaadr(SchemeObject* o) { return s_cxr(o, "daaa"); };
SchemeObject* s_caadar(SchemeObject* o) { return s_cxr(o, "adaa"); };
SchemeObject* s_caaddr(SchemeObject* o) { return s_cxr(o, "ddaa"); };
SchemeObject* s_cadaar(SchemeObject* o) { return s_cxr(o, "aada"); };
SchemeObject* s_cadadr(SchemeObject* o) { return s_cxr(o, "dada"); };
SchemeObject* s_caddar(SchemeObject* o) { return s_cxr(o, "adda"); };
SchemeObject* s_cadddr(SchemeObject* o) { return s_cxr(o, "ddda"); };
SchemeObject* s_cdaaar(SchemeObject* o) { return s_cxr(o, "aaad"); };
SchemeObject* s_cdaadr(SchemeObject* o) { return s_cxr(o, "daad"); };
SchemeObject* s_cdadar(SchemeObject* o) { return s_cxr(o, "adad"); };
SchemeObject* s_cdaddr(SchemeObject* o) { return s_cxr(o, "ddad"); };
SchemeObject* s_cddaar(SchemeObject* o) { return s_cxr(o, "aadd"); };
SchemeObject* s_cddadr(SchemeObject* o) { return s_cxr(o, "dadd"); };
SchemeObject* s_cdddar(SchemeObject* o) { return s_cxr(o, "addd"); };
SchemeObject* s_cddddr(SchemeObject* o) { return s_cxr(o, "dddd"); };

// (cons a b)
SchemeObject* s_cons(SchemeObject* car, SchemeObject* cdr) {
    return i_cons(car, cdr);
}

SchemeObject* s_list(int num, SchemeStack::iterator args) {
    SchemeObject* result = S_EMPTY_LIST;        
    for(int i = 0; i < num; i++) {
        result = i_cons(args[num-1-i], result);    
    }
    return result;
}

SchemeObject* s_reverse(SchemeObject* o) 
{
    SchemeObject* result = S_EMPTY_LIST;

    while (o != S_EMPTY_LIST) {
        assert_arg_pair_type(L"reverse", 1, o);
	result = i_cons(i_car(o), result);
	o = i_cdr(o);
    }
    return result;  
}

SchemeObject* s_length(SchemeObject* p) {
    int length = 0;
    while (p != S_EMPTY_LIST) {
        assert_arg_pair_type(L"length", 1, p);
        length++;
        p = i_cdr(p);
    }
    return int2scm(length);
}

SchemeObject* s_set_car_e(SchemeObject* p, SchemeObject* o) {
    assert_arg_pair_type(L"set-car!", 1, p);
    assert_arg_not_immutable(L"set-car!", 1, p);
    i_set_car_e(p, o);
    return S_UNSPECIFIED;
}

SchemeObject* s_set_cdr_e(SchemeObject* p, SchemeObject* o) {
    assert_arg_pair_type(L"set-cdr!", 1, p);
    assert_arg_not_immutable(L"set-cdr!", 1, p);
    i_set_cdr_e(p, o);
    return S_UNSPECIFIED;
}

SchemeObject* s_append(int num, SchemeStack::iterator stack) {
    SchemeObject* result = S_EMPTY_LIST;
    SchemeObject* result_tail = NULL;
    
    if (num == 0) {
        return S_EMPTY_LIST;
    }
    for(int i = 0; i < num-1; i++) {
        SchemeObject* pp = *stack++;
        // Skip empty lists
        if (pp != S_EMPTY_LIST) {
            assert_arg_type(L"append", i, s_pair_p, pp);

    	    while (pp != S_EMPTY_LIST) {

                assert_arg_type(L"append", i, s_pair_p, pp);

    	        SchemeObject* newtail = s_cons(s_car(pp), S_EMPTY_LIST);
    	        if (result == S_EMPTY_LIST) {
                    result = newtail;
                    result_tail = result;
    	        } else {
                    i_set_cdr_e(result_tail, newtail);
                    result_tail = newtail;
                }

                pp = s_cdr(pp);
    	    }
	}
    }

    // Append  final arg
    if (result != S_EMPTY_LIST) {
        i_set_cdr_e(result_tail, *stack);
    } else {
        result = *stack;
    }
    return result;
}

void coerceNumbers(int num, SchemeStack::iterator stack, double* dest) {
    for(int i = 0; i < num; i++) {
        *dest = scm2double(*stack);
        stack++;
        dest++;
    }
}

void coerceNumbers(int num, SchemeStack::iterator stack, long* dest) {
    for(int i = 0; i < num; i++) {
        *dest = scm2int(*stack);
        stack++;
        dest++;
    }
}

void coerceNumbers(int num, SchemeStack::iterator stack, pair<long,long>* dest) {
    for(int i = 0; i < num; i++) {
        *dest = scm2rational(*stack);
        stack++;
        dest++;
    }
}

void coerceNumbers(int num, SchemeStack::iterator stack, std::complex<double>* dest) {
    for(int i = 0; i < num; i++) {
        *dest = scm2complex(*stack);
        stack++;
        dest++;
    }
}


// This returns the best number type for the numbers in stacks. Ie. if the stack contains all integers
// but just one rational, the rational is returned. If it's all rationals but with just one real, the
// real-type is returned. 
SchemeObject::ObjectType representativeNumberType(wchar_t* procname, int num, SchemeStack::iterator stack) {
    SchemeObject::ObjectType type = SchemeObject::INTEGER_NUMBER;
    for(int i = 0; i < num; i++) {
        SchemeObject* n = *stack;
        assert_arg_number_type(procname, i+1, n);
        if (n->type() < type) {
            type = n->type();        
        }
        stack++;    
    }
    return type;
}

SchemeObject* s_plus(int num, SchemeStack::iterator stack) {
    SchemeObject::ObjectType outType = representativeNumberType(L"+", num, stack);

    if (outType == SchemeObject::REAL_NUMBER) {
       double args[num];
       coerceNumbers(num,stack,args);
       
       double result = 0;
       for(int i = 0; i < num; i++) {
           result += args[i];    
       }
       return double2scm(result);
    } else if (outType == SchemeObject::INTEGER_NUMBER) {
       long args[num];
       coerceNumbers(num,stack,args);
       
       long result = 0;
       for(int i = 0; i < num; i++) {
           result += args[i];    
       }
       return int2scm(result);
    } else if (outType == SchemeObject::COMPLEX_NUMBER) {
       std::complex<double> args[num];
       coerceNumbers(num,stack,args);
       
       std::complex<double> result = 0;
       for(int i = 0; i < num; i++) {
           result += args[i];    
       }
       return complex2scm(result);
    } else if (outType == SchemeObject::RATIONAL_NUMBER) {
       pair<long,long> args[num];
       coerceNumbers(num,stack,args);
       
       // Using a/b + c/d = (ad + bc)/bd
       pair<long,long> result(0,1);
       for(int i = 0; i < num; i++) {
           long n = result.first * args[i].second + result.second * args[i].first;
           long d = result.second * args[i].second;
           result.first = n;
           result.second = d;    
       }
       return rational2scm(result);
    } else {
        throw scheme_exception(L"+", L"Only integer and real support");    
    }
}

SchemeObject* s_minus(int num, SchemeStack::iterator stack) {
    SchemeObject::ObjectType outType = representativeNumberType(L"-", num, stack);
    if (outType == SchemeObject::REAL_NUMBER) {
       double args[num];
       coerceNumbers(num,stack,args);
       double result = args[0];
       if (num == 1) {
           // One-argument case is a simple negate (n => -n)
           return double2scm(-result);
       } 
       for(int i = 1; i < num; i++) {
           result -= args[i];    
       }
       return double2scm(result);
    } else if (outType == SchemeObject::INTEGER_NUMBER) {
       long args[num];
       coerceNumbers(num,stack,args);
       long result = args[0];
       if (num == 1) {
           // One-argument case is a simple negate (n => -n)
           return int2scm(-result);
       } 
       for(int i = 1; i < num; i++) {
           result -= args[i];    
       }
       return int2scm(result);
    } else if (outType == SchemeObject::COMPLEX_NUMBER) {
       std::complex<double> args[num];
       coerceNumbers(num,stack,args);
       std::complex<double> result = args[0];
       if (num == 1) {
           // One-argument case is a simple negate (n => -n)
           return complex2scm(-result);
       } 
       for(int i = 1; i < num; i++) {
           result -= args[i];    
       }
       return complex2scm(result);
    } else if (outType == SchemeObject::RATIONAL_NUMBER) {
       pair<long,long> args[num];
       coerceNumbers(num,stack,args);
       
       pair<long,long> result = args[0];
       if (num == 1) {
           result.first = -result.first;
           return rational2scm(result);
       }
       
       // Using a/b - c/d = (ad - bc)/bd
       for(int i = 1; i < num; i++) {
           long n = result.first * args[i].second - result.second * args[i].first;
           long d = result.second * args[i].second;
           result.first = n;
           result.second = d;    
       }
       return rational2scm(result);
    } else {
        throw scheme_exception(L"-", L"Only integer and real support");    
    }        
}

SchemeObject* s_divide(int num, SchemeStack::iterator stack) {
    SchemeObject::ObjectType outType = representativeNumberType(L"/", num, stack);
    if (outType == SchemeObject::REAL_NUMBER) {
       double args[num];
       coerceNumbers(num,stack,args);
       double result = args[0];
       if (num == 1) {
           // One-argument case is a simple inverse (n => 1/n)
           return double2scm(1.0 / result);
       } 
       for(int i = 1; i < num; i++) {
           result /= args[i];    
       }
       return double2scm(result);
    } else if (outType == SchemeObject::COMPLEX_NUMBER) {
       std::complex<double> args[num];
       coerceNumbers(num,stack,args);
       std::complex<double> result = args[0];
       if (num == 1) {
           // One-argument case is a simple inverse (n => 1/n)
           return complex2scm(1.0 / result);
       } 
       for(int i = 1; i < num; i++) {
           result /= args[i];    
       }
       return complex2scm(result);
    } else {
       // Both integers and rational types are handled as rationals            
       pair<long,long> args[num];
       coerceNumbers(num,stack,args);

       pair<long,long> result = args[0];
       if (num == 1) {
           // One-argument case is a simple inverse (n => 1/n)
           // which for rationals is a swap of numerator and denominator
           long tmp = result.first;
           result.first = result.second;
           result.second = tmp;
           return rational2scm(result);
       }

       // Using a/b / c/d = ad/bc
       for(int i = 1; i < num; i++) {
           long n = result.first * args[i].second;
           long d = result.second * args[i].first;
           result.first = n;
           result.second = d;    
       }
       return rational2scm(result);
   }
}

SchemeObject* s_mult(int num, SchemeStack::iterator stack) {
    SchemeObject::ObjectType outType = representativeNumberType(L"*", num, stack);

    if (outType == SchemeObject::REAL_NUMBER) {
       double args[num];
       coerceNumbers(num,stack,args);
       
       double result = 1;
       for(int i = 0; i < num; i++) {
           result *= args[i];    
       }
       return double2scm(result);
            
    } else if (outType == SchemeObject::INTEGER_NUMBER) {
       long args[num];
       coerceNumbers(num,stack,args);
       
       long result = 1;
       for(int i = 0; i < num; i++) {
           result *= args[i];    
       }
       return int2scm(result);
    } else if (outType == SchemeObject::COMPLEX_NUMBER) {
       std::complex<double> args[num];
       coerceNumbers(num,stack,args);
       
       std::complex<double> result = 1;
       for(int i = 0; i < num; i++) {
           result *= args[i];    
       }
       return complex2scm(result);
    } else if (outType == SchemeObject::RATIONAL_NUMBER) {
       pair<long,long> args[num];
       coerceNumbers(num,stack,args);
       
       // Using a/b * c/d = ac/bd
       pair<long,long> result(1,1);
       for(int i = 0; i < num; i++) {
           result.first *= args[i].first;
           result.second *= args[i].second;
       }
       return rational2scm(result);
    } else {
        throw scheme_exception(L"*", L"Only integer and real support");    
    }
}

SchemeObject* s_make_vector(SchemeObject* s_count, SchemeObject* obj) {
    assert_arg_positive_int(L"make-vector", 1, s_count);
    int count = scm2int(s_count);
    return SchemeObject::createVector(obj, count);
}

SchemeObject* s_vector(int num, SchemeStack::iterator args) {
    SchemeObject* result = SchemeObject::createVector(S_UNSPECIFIED, num);
    for(int i = 0; i < num; i++) {
        result->setVectorElem(args[i], i);
    }
    return result;
}

SchemeObject* s_vector_length(SchemeObject* v) {
    assert_arg_type(L"vector-length", 1, s_vector_p, v);
    return int2scm(v->length);
}

SchemeObject* s_list_2_vector(SchemeObject* l) {
    assert_arg_type(L"list->vector", 1, s_list_p, l);
    int c = scm2int(s_length(l));
    SchemeObject* result = SchemeObject::createVector(S_UNSPECIFIED, c);
    int i = 0;
    while (l != S_EMPTY_LIST) {
        result->setVectorElem(i_car(l), i++);
        l = i_cdr(l);
    }
    return result;
}

SchemeObject* s_vector_2_list(SchemeObject* v) {
    assert_arg_type(L"vector->list", 1, s_vector_p, v);
    SchemeObject* result = S_EMPTY_LIST;
    for(int i = v->length-1; i >= 0; i--) {
	result = i_cons(v->getVectorElem(i), result);
    }
    return result;
}

SchemeObject* s_vector_ref(SchemeObject* s_v, SchemeObject* s_index) {
    assert_arg_type(L"vector-ref", 1, s_vector_p, s_v);
    assert_arg_int_in_range(L"vector-ref", 2, s_index, 0, s_v->length-1);
    int i = scm2int(s_index);
    return s_v->getVectorElem(i);
}

SchemeObject* s_vector_set_e(SchemeObject* s_vec, SchemeObject* s_index, SchemeObject* val) {
    assert_arg_type(L"vector-set!", 1, s_vector_p, s_vec);
    assert_arg_not_immutable(L"vector-set!", 1, s_vec);
    assert_arg_int_in_range(L"vector-set!", 2, s_index, 0, s_vec->length-1);
    uint32_t i = scm2int(s_index);
    s_vec->setVectorElem(val, i);
    return S_UNSPECIFIED;
}

SchemeObject* s_vector_fill_e(SchemeObject* s_vec, SchemeObject* fill) {
    assert_arg_type(L"vector-fill!", 1, s_vector_p, s_vec);
    assert_arg_not_immutable(L"vector-fill!", 1, s_vec);
    for(int i = 0; i < s_vec->length; i++) {
	s_vec->setVectorElem(fill, i);
    }
    return S_UNSPECIFIED;
}

SchemeObject* s_sqrt(SchemeObject* n) {
    assert_arg_number_type(L"sqrt", 1, n);
    if (n->type() == SchemeObject::COMPLEX_NUMBER) {
        return complex2scm(std::sqrt(scm2complex(n)));
    } else {
        double d = scm2double(n);
        if (d < 0) {
            return complex2scm(std::sqrt(scm2complex(n)));
        } else {
            return double2scm(sqrt(scm2double(n)));
        }
    }
}

SchemeObject* s_abs(SchemeObject* n) {
    if (n->type() == SchemeObject::COMPLEX_NUMBER) {
        return double2scm(std::abs(scm2complex(n)));
    } else if (n->type() == SchemeObject::REAL_NUMBER) {
        return double2scm(fabs(scm2double(n)));
    } else if (n->type() == SchemeObject::INTEGER_NUMBER) {
        return int2scm(labs(scm2int(n)));
    } else {
        wrong_type_arg(L"abs", 1, n);    
    }         
}


SchemeObject* s_sin(SchemeObject* n) {
    assert_arg_number_type(L"sin", 1, n);
    if (n->type() == SchemeObject::COMPLEX_NUMBER) {
        return complex2scm(std::sin(scm2complex(n)));
    } else {
        return double2scm(::sin(scm2double(n)));
    }
}

SchemeObject* s_asin(SchemeObject* n) {
    assert_arg_number_type(L"asin", 1, n);
    if (n->type() == SchemeObject::COMPLEX_NUMBER) {
        complex<double> i = complex<double>(0,1);    
        complex<double> z = scm2complex(n);
        // Using the formula from R^5RS
        return complex2scm(-i * std::log(i*z + std::sqrt(1.0-z*z)));     
    } else {
        return double2scm(::asin(scm2double(n)));
    }
}

SchemeObject* s_cos(SchemeObject* n) {
    assert_arg_number_type(L"cos", 1, n);
    if (n->type() == SchemeObject::COMPLEX_NUMBER) {
        return complex2scm(std::cos(scm2complex(n)));
    } else {
        return double2scm(::cos(scm2double(n)));
    }
}

SchemeObject* s_acos(SchemeObject* n) {
    assert_arg_number_type(L"acos", 1, n);
    if (n->type() == SchemeObject::COMPLEX_NUMBER) {
        complex<double> i = complex<double>(0,1);    
        complex<double> z = scm2complex(n);
        // Using the formula from R^5RS    
        complex<double> asin_z = (-i) * std::log(i*z + std::sqrt(1.0-z*z));     
        return complex2scm(M_PI/2.0 - asin_z);
    } else {
        return double2scm(::acos(scm2double(n)));
    }
}

SchemeObject* s_tan(SchemeObject* n) {
    assert_arg_number_type(L"tan", 1, n);
    if (n->type() == SchemeObject::COMPLEX_NUMBER) {
        return complex2scm(std::tan(scm2complex(n)));
    } else {
        return double2scm(::tan(scm2double(n)));
    }
}

SchemeObject* s_atan(SchemeObject* y, SchemeObject* x) {
    assert_arg_number_type(L"atan", 1, y);
    if (x == S_UNSPECIFIED) {
        if (y->type() == SchemeObject::COMPLEX_NUMBER) {
            complex<double> i = complex<double>(0,1);    
            complex<double> z = scm2complex(y);
            // Using the formula from R^5RS    
            return complex2scm((std::log(1.0+i*z) - std::log(1.0-i*z)) / (2.0*i));
        } else {
            return double2scm(::atan(scm2double(y)));
        }
    } else {
        assert_arg_type(L"atan", 1, s_real_p, y);
        assert_arg_type(L"atan", 2, s_real_p, x);
        return double2scm(::atan2(scm2double(y), scm2double(x)));
    }
}

// In R6RS log takes an optional base argument, which defaults to e for the natural logarithm.
// Note that log_b(x) = log_k(x) / log_k(b) for any base k
// For example log2(16) = log(16) / log(2) 
// TODO: Implement
SchemeObject* s_log(SchemeObject* n) {
    assert_arg_number_type(L"log", 1, n);
    if (n->type() == SchemeObject::COMPLEX_NUMBER) {
        return complex2scm(std::log(scm2complex(n)));
    } else {
        return double2scm(::log(scm2double(n)));
    }
}

// Returns a^b
SchemeObject* s_expt(SchemeObject* a, SchemeObject* b) {
    if (a->type() == SchemeObject::INTEGER_NUMBER && b->type() == SchemeObject::INTEGER_NUMBER && scm2int(b) >= 0) {
        // TODO: Find a better algorithm for the integer case
        long ai = scm2int(a);    
        long bi = scm2int(b);    
        if (bi == 0) return S_ONE;
        long result = 1;
        while (bi-- > 0) {
            result *= ai;        
        }
        return int2scm(result);
    } else {
        assert_arg_number_type(L"expt", 1, a);
        assert_arg_number_type(L"expt", 2, b);
        bool a_c = a->type() == SchemeObject::COMPLEX_NUMBER;
        bool b_c = b->type() == SchemeObject::COMPLEX_NUMBER;
        
        if (a_c) {
            if (b_c) {
                return complex2scm(std::pow(scm2complex(a), scm2complex(b)));        
            } else {
                if (b->type() == SchemeObject::INTEGER_NUMBER) {
                    return complex2scm(std::pow(scm2complex(a), scm2int(b)));
                } else {   
                    return complex2scm(std::pow(scm2complex(a), scm2double(b)));        
                }
            }        
        } else {
            if (b_c) {
                return complex2scm(std::pow(scm2double(a), scm2complex(b)));        
            } else {
                return double2scm(pow(scm2double(a),scm2double(b)));
            }           
        }
    }
}

// Returns e^n
SchemeObject* s_exp(SchemeObject* n) {
    assert_arg_number_type(L"exp", 1, n);
    if (n->type() == SchemeObject::COMPLEX_NUMBER) {
        return complex2scm(std::exp(scm2complex(n)));
    } else {
        return double2scm(::exp(scm2double(n)));
    }
}

// Round returns the closest integer to x, rounding to even when x is halfway between two integers.
SchemeObject* s_round(SchemeObject* n) {
    if (n->type() == SchemeObject::INTEGER_NUMBER) return n;
    assert_arg_type(L"round", 1, s_real_p, n);
    double nn = scm2double(n);
    double flo = floor(nn);
    double cei = ceil(nn);
    double dflo = nn - flo;
    double dcei = cei - nn;
    double result;
    if (dflo > dcei) {
        result = cei;
    } else if (dcei > dflo) {
        result = flo;
    } else {
        if(fmod(flo, 2) == 0) {
             result = flo;
        } else {
             result = cei;
        }
    }
    return double2scm(result);
}

// Ceiling returns the smallest integer not smaller than x
SchemeObject* s_ceiling(SchemeObject* n) {
    if (n->type() == SchemeObject::INTEGER_NUMBER) {
        return n;
    } else if (n->type() == SchemeObject::RATIONAL_NUMBER) {
        pair<long,long> z = scm2rational(n);
        long numerator = z.first;
        long denominator = z.second;
        long quotient = numerator / denominator;
        if (quotient >= 0 && numerator > 0) {
            return int2scm(quotient+1);    
        } else {
            return int2scm(quotient);
        }
    } else {
        assert_arg_type(L"ceiling", 1, s_real_p, n);
        return double2scm(ceil(scm2double(n)));
    }
}

// Floor returns the largest integer not larger than x
SchemeObject* s_floor(SchemeObject* n) {
    if (n->type() == SchemeObject::INTEGER_NUMBER) {
        return n;
    } else if (n->type() == SchemeObject::RATIONAL_NUMBER) {
        pair<long,long> z = scm2rational(n);
        long numerator = z.first;
        long denominator = z.second;
        long quotient = numerator / denominator;
        if (quotient >= 0 && numerator > 0) {
            return int2scm(quotient);    
        } else {
            return int2scm(quotient-1);
        }
    } else {
        assert_arg_type(L"floor", 1, s_real_p, n);
        return double2scm(floor(scm2double(n)));
    }
}

// Truncate returns the integer closest to x whose absolute value is not larger than the absolute value of x
SchemeObject* s_truncate(SchemeObject* n) {
    if (n->type() == SchemeObject::INTEGER_NUMBER) {
        return n;
    } else if (n->type() == SchemeObject::RATIONAL_NUMBER) {
        pair<long,long> z = scm2rational(n);
        long numerator = z.first;
        long old_numerator = numerator;
        long denominator = z.second;
        if (old_numerator < 0) numerator = -numerator;
        long quotient = numerator / denominator;
        long flo;
        if (quotient >= 0 && numerator > 0) {
            flo = quotient;    
        } else {
            flo = quotient-1;
        }
        if (old_numerator < 0) {
            flo = -flo;        
        }
        return int2scm(flo);    
    } else {
        assert_arg_type(L"truncate", 1, s_real_p, n);
        double t = scm2double(n);
        #if HAVE_TRUNC
            double r = trunc(t);
        #else
            double r = t < 0 ? -floor(-t) : floor(t);
        #endif    
        return double2scm(r);
    }        
}

SchemeObject* s_quotient(SchemeObject* n1, SchemeObject* n2) {
    assert_arg_type(L"quotient", 1, s_integer_p, n1);
    assert_arg_type(L"quotient", 2, s_integer_p, n2);
    long nn1 = scm2int(n1);
    long nn2 = scm2int(n2);
    long result = nn1 / nn2;
    if (s_inexact_p(n1) == S_TRUE || s_inexact_p(n2) == S_TRUE) {
        return double2scm(double(result));
    } else {
        return int2scm(result);
    }
}

SchemeObject* s_remainder(SchemeObject* n1, SchemeObject* n2) {
    assert_arg_type(L"remainder", 1, s_integer_p, n1);
    assert_arg_type(L"remainder", 2, s_integer_p, n2);
    long nn1 = scm2int(n1);
    long nn2 = scm2int(n2);
    long result = nn1 % nn2;
    if (result > 0) {
        if (nn1 < 0) {
            result -= labs(nn2);
        }
    } else if (result < 0) {
        if (nn1 > 0) {
            result += labs(nn2);
        }
    }
    if (s_inexact_p(n1) == S_TRUE || s_inexact_p(n2) == S_TRUE) {
        return double2scm(double(result));
    } else {
        return int2scm(result);
    }
}

SchemeObject* s_modulo(SchemeObject* n1, SchemeObject* n2) {
    assert_arg_type(L"modulo", 1, s_integer_p, n1);
    assert_arg_type(L"modulo", 2, s_integer_p, n2);
    long nn1 = scm2int(n1);
    long nn2 = scm2int(n2);
    long result = nn1 % nn2;
    if (result * nn2 < 0) {
        if (result > 0) {
            result -= labs(nn2);
        } else {
            result += labs(nn2);
        }
    }
    if (s_inexact_p(n1) == S_TRUE || s_inexact_p(n2) == S_TRUE) {
        return double2scm(double(result));
    } else {
        return int2scm(result);
    }
}


SchemeObject* s_min(int num, SchemeStack::iterator stack) {
    assert (num > 0);
    SchemeObject::ObjectType outType = representativeNumberType(L"min", num, stack);

    if (outType == SchemeObject::REAL_NUMBER) {
        double args[num];
        coerceNumbers(num,stack,args);
        double result = args[0];
        for(int i = 1; i < num; i++) {
            if (args[i] < result) {
                result = args[i];         
            }    
        }
        return double2scm(result);
    } else if (outType == SchemeObject::INTEGER_NUMBER) {
       long args[num];
       coerceNumbers(num,stack,args);
       long result = args[0];
       int index = 0;
       for(int i = 1; i < num; i++) {
           if (args[i] < result) {
               result = args[i];
               index = i;
           }    
       }
       return stack[index];
    } else {
        throw scheme_exception(L"+", L"Only integer and real support");    
    }
}

SchemeObject* s_max(int num, SchemeStack::iterator stack) {
    assert (num > 0);
    SchemeObject::ObjectType outType = representativeNumberType(L"max", num, stack);

    if (outType == SchemeObject::REAL_NUMBER) {
        double args[num];
        coerceNumbers(num,stack,args);
        double result = args[0];
        for(int i = 1; i < num; i++) {
            if (args[i] > result) {
                result = args[i];         
            }    
        }
        return double2scm(result);
    } else if (outType == SchemeObject::INTEGER_NUMBER) {
       long args[num];
       coerceNumbers(num,stack,args);
       long result = args[0];
       int index = 0;
       for(int i = 1; i < num; i++) {
           if (args[i] > result) {
               result = args[i];
               index = i;
           }    
       }
       return stack[index];
    } else {
        throw scheme_exception(L"+", L"Only integer and real support");    
    }
}

long i_gcd(long a, long b) {
    long t = a;
    while(b != 0) {
        t = b;
        b = a % b;
        a = t;
    }
    return t;
}

// Using Euclids algorithm and that gcd is associative thus gcd(a,b,c) = gcd(a,(gcd(b,c))) = gcd(gcd(a,b),c).
SchemeObject* s_gcd(int num, SchemeStack::iterator stack) {
    if (num == 0) {
        return S_ZERO;
    }
    assert_arg_int_type(L"gcd", 1, *stack); // This 1 is wrong as we s_gcd is recursive with descreasing num
    if (num == 1) {
        return int2scm(labs(scm2int(*stack)));
    }
    long a = scm2int(*stack);
    long b = scm2int(s_gcd(num-1, ++stack));
    return int2scm(labs(i_gcd(a,b)));
}

// Using the property gcd(a,b) * lcm(a,b) = a * b and that lcm(a,b,c) = lcm(lcm(a,b),c) = lcm(a,lcm(b,c))
SchemeObject* s_lcm(int num, SchemeStack::iterator stack) {
    if (num == 0) {
        return S_ONE;
    }
    if (num == 1) {
        assert_arg_int_type(L"lcm", 1, *stack); // This 1 is wrong as we s_gcd is recursive with descreasing num
        return int2scm(labs(scm2int(*stack)));
    }

    long a = labs(scm2int(*stack));
    long b = labs(scm2int(s_lcm(num-1, ++stack)));
    long g = i_gcd(a,b);
    long r;
    if (g == 0) {
        r = 0;
    } else {
        r = a * b / g;
    }
    return int2scm(r);
}

// Reduce a rational to its normalized form, which is
// 1) denominator > 0.
// 2) commons terms are cancelled out.
// 3) throw exception if denominator is zero.
void i_normalize_rational(long* numerator, long* denominator) {
    if (*denominator == 0) {
        throw scheme_exception(L"Denominator is zero in rational");    
    }        
    long gcd = i_gcd(*numerator, *denominator);
    if (gcd != 1) {
        *numerator /= gcd;
        *denominator /= gcd;
    }
    if (*denominator < 0) {
        *numerator *= -1;
        *denominator *= -1;
    }
}

SchemeObject* s_numerator(SchemeObject* n) {
    assert_arg_type(L"numerator", 1, s_real_p, n);
    if (s_exact_p(n) == S_TRUE) {
        return int2scm(n->rationalValue().first);
    } else {
        SchemeObject* z = s_inexact_2_exact(n);
        long l = z->rationalValue().first;
        return double2scm(double(l));
    }
}

SchemeObject* s_denominator(SchemeObject* n) {
    assert_arg_type(L"denominator", 1, s_real_p, n);
    if (s_exact_p(n) == S_TRUE) {
        return int2scm(n->rationalValue().second);
    } else {
        SchemeObject* z = s_inexact_2_exact(n);
        long l = z->rationalValue().second;
        return double2scm(double(l));
    }
}

SchemeObject* s_make_polar(SchemeObject* magnitude, SchemeObject* angle) {
    assert_arg_type(L"make-polar", 1, s_real_p, magnitude);
    assert_arg_type(L"make-polar", 2, s_real_p, angle);
    std::complex<double> z = std::polar(magnitude->realValue(), angle->realValue());
    return complex2scm(z);
}

SchemeObject* s_make_rectangular(SchemeObject* real, SchemeObject* imag) {
    assert_arg_type(L"make-rectangular", 1, s_real_p, real);
    assert_arg_type(L"make-rectangular", 2, s_real_p, imag);
    std::complex<double> z(real->realValue(), imag->realValue());
    return complex2scm(z);
}

SchemeObject* s_real_part(SchemeObject* z) {
    assert_arg_complex_type(L"real-part", 1, z);
    std::complex<double> zz = z->complexValue();
    return double2scm(zz.real());
}

SchemeObject* s_imag_part(SchemeObject* z) {
    assert_arg_complex_type(L"imag-part", 1, z);
    std::complex<double> zz = z->complexValue();
    return double2scm(zz.imag());
}

SchemeObject* s_magnitude(SchemeObject* z) {
    assert_arg_complex_type(L"magnitude", 1, z);
    std::complex<double> zz = z->complexValue();
    return double2scm(std::abs(zz));        
}

SchemeObject* s_angle(SchemeObject* z) {
    assert_arg_complex_type(L"angle", 1, z);
    std::complex<double> zz = z->complexValue();
    return double2scm(::atan2(zz.imag(), zz.real()));
}

SchemeObject* s_even_p(SchemeObject* n) {
    assert_arg_int_type(L"even?", 1, n);
    return (scm2int(n) & 0x1) == 0 ? S_TRUE : S_FALSE;
}

SchemeObject* s_odd_p(SchemeObject* n) {
    assert_arg_int_type(L"odd?", 1, n);
    return (scm2int(n) & 0x1) == 1 ? S_TRUE : S_FALSE;
}

SchemeObject* s_zero_p(SchemeObject* n) {
    assert_arg_number_type(L"zero?", 1, n);
    bool result;
    if (n->type() == SchemeObject::INTEGER_NUMBER) {
        result = scm2int(n) == 0;    
    } else if (n->type() == SchemeObject::RATIONAL_NUMBER) {
        result = scm2int(n->numerator) == 0;
    } else if (n->type() == SchemeObject::COMPLEX_NUMBER) {
        std::complex<double> c = n->complexValue();
        result = c.real() == 0.0 && c.imag() == 0.0;
    } else {
        result = scm2double(n) == 0.0;
    }
    return bool2scm(result);
}

SchemeObject* s_negative_p(SchemeObject* n) {
    assert_arg_type(L"negative?", 1, s_real_p, n);
    bool result;
    if (n->type() == SchemeObject::INTEGER_NUMBER) {
        result = scm2int(n) < 0;    
    } else if (n->type() == SchemeObject::RATIONAL_NUMBER) {
        result = scm2int(n->numerator) < 0;
    } else {
        result = scm2double(n) < 0;
    }
    return bool2scm(result);
}

SchemeObject* s_positive_p(SchemeObject* n) {
    assert_arg_type(L"positive?", 1, s_real_p, n);
    bool result;
    if (n->type() == SchemeObject::INTEGER_NUMBER) {
        result = scm2int(n) > 0;    
    } else if (n->type() == SchemeObject::RATIONAL_NUMBER) {
        result = scm2int(n->numerator) > 0;
    } else {
        result = scm2double(n) > 0;
    }
    return bool2scm(result);
}

// (= a b)
// FIXME: Numerical instable. Use integer and rational comparisons
SchemeObject* s_equal(int num, SchemeStack::iterator args) {
    if (num == 0) {
        return S_TRUE;
    }
    assert_arg_type(L"=", 1, s_real_p, *args);
    double n = scm2double(*args);
    args ++;
    for(int i = 1; i < num; i++) {
        SchemeObject* v = *args++;
        assert_arg_type(L"=", i+1, s_real_p, v);
        double nn = scm2double(v);
        if (nn != n) {
            return S_FALSE;
        }
        n = nn;
    }
    return S_TRUE;
}

// FIXME: Numerical instable. Use integer and rational comparisons
SchemeObject* s_less(int num, SchemeStack::iterator args) {
    if (num == 0) {
        return S_TRUE;
    }
    assert_arg_type(L"<", 1, s_real_p, *args);
    double n = scm2double(*args);
    args ++;
    for(int i = 1; i < num; i++) {
        SchemeObject* v = *args++;
        assert_arg_type(L"<", i+1, s_real_p, v);
        double nn = scm2double(v);
        if (nn <= n) {
            return S_FALSE;
        }
        n = nn;
    }
    return S_TRUE;
}

// FIXME: Numerical instable. Use integer and rational comparisons
SchemeObject* s_less_equal(int num, SchemeStack::iterator args) {
    if (num == 0) {
        return S_TRUE;
    }
    assert_arg_type(L"<=", 1, s_real_p, *args);
    double n = scm2double(*args);
    args ++;
    for(int i = 1; i < num; i++) {
        SchemeObject* v = *args++;
        assert_arg_type(L"<=", i+1, s_real_p, v);
        double nn = scm2double(v);
        if (nn < n) {
            return S_FALSE;
        }
        n = nn;
    }
    return S_TRUE;
}

// FIXME: Numerical instable. Use integer and rational comparisons
SchemeObject* s_greater(int num, SchemeStack::iterator args) {        
    if (num == 0) {
        return S_TRUE;
    }
    assert_arg_type(L">", 1, s_real_p, *args);
    double n = scm2double(*args);
    args ++;
    for(int i = 1; i < num; i++) {
        SchemeObject* v = *args++;
        assert_arg_type(L">", i+1, s_real_p, v);
        double nn = scm2double(v);
        if (nn >= n) {
            return S_FALSE;
        }
        n = nn;
    }
    return S_TRUE;
}

// FIXME: Numerical instable. Use integer and rational comparisons
SchemeObject* s_greater_equal(int num, SchemeStack::iterator args) {
    if (num == 0) {
        return S_TRUE;
    }
    assert_arg_type(L">=", 1, s_real_p, *args);
    double n = scm2double(*args);
    args ++;
    for(int i = 1; i < num; i++) {
        SchemeObject* v = *args++;
        assert_arg_type(L">=", i+1, s_real_p, v);
        double nn = scm2double(v);
        if (nn > n) {
            return S_FALSE;
        }
        n = nn;
    }
    return S_TRUE;
}

SchemeObject* s_not(SchemeObject* o) {
    return o == S_FALSE ? S_TRUE : S_FALSE;
}

SchemeObject* s_make_string(SchemeObject* len, SchemeObject* chr) {
    assert_arg_type(L"make-string", 1, s_integer_p, len);
    
    if (chr == S_UNSPECIFIED) {
        chr = S_SPACE;
    } else {
        assert_arg_type(L"make-string", 2, s_char_p, chr);
    }

    wstring s = wstring(scm2int(len), scm2char(chr));
    return string2scm(s);
}

SchemeObject* s_string(int num, SchemeStack::iterator args) {
    wstring s = L"";
    for(int i = 0; i < num; i++) {
        SchemeObject* c = args[i];
        assert_arg_type(L"string", i, s_char_p, c);
        s += scm2char(c);
    }
    return string2scm(s);
}

SchemeObject* s_string_length(SchemeObject* s) {
    assert_arg_type(L"string-length", 1, s_string_p, s);
    int len = s->length;
    return int2scm(len);
}

SchemeObject* s_string_ref(SchemeObject* s, SchemeObject* i) {
    assert_arg_type(L"string-ref", 1, s_string_p, s);
    assert_arg_int_in_range(L"string-ref", 2, i, 0, s->length-1);
    int index = scm2int(i);
    return char2scm(s->str[index]);
}	

SchemeObject* s_string_set_e(SchemeObject* s, SchemeObject* i, SchemeObject* chr) {
    assert_arg_type(L"string-set!", 1, s_string_p, s);
    assert_arg_type(L"string-set!", 3, s_char_p, chr);
    assert_arg_not_immutable(L"string-set!", 1, s);
    assert_arg_int_in_range(L"string-set!", 2, i, 0, s->length-1);
    s->str[scm2int(i)] = scm2char(chr);
    return S_UNSPECIFIED;
}

SchemeObject* s_symbol_2_string(SchemeObject* symbol) {
    assert_arg_type(L"symbol->string", 1, s_symbol_p, symbol);
    SchemeObject* result = SchemeObject::createString(symbol->str);
    // TODO: String constructor shouldn't strdup the string. Just reuse point.
    // That's why we mark it immutable.
    result->set_immutable(true);
    return result;
}

SchemeObject* s_string_2_symbol(SchemeObject* s) {
    assert_arg_type(L"string->symbol", 1, s_string_p, s);
    return SchemeObject::createSymbol(s->str);
}

SchemeObject* s_string_append(int num, SchemeStack::iterator args) {
    wstring result = L"";
    for(int i = 0; i < num; i++, args++) {
        assert_arg_type(L"string-append", i+1, s_string_p, *args);
        result += scm2string(*args);
    }
    return string2scm(result);
}

SchemeObject* s_string_copy(SchemeObject* str) {
    assert_arg_type(L"string-copy", 1, s_string_p, str);
    return cstr2scm(str->str);
}

SchemeObject* s_substring(SchemeObject* s_str, SchemeObject* s_start, SchemeObject* s_end) {
    assert_arg_type(L"substring", 1, s_string_p, s_str);
    assert_arg_type(L"substring", 2, s_integer_p, s_start);
    assert_arg_type(L"substring", 3, s_integer_p, s_end);
    wstring str = scm2string(s_str);
    int start = scm2int(s_start);
    int end = scm2int(s_end);
    int len = str.size();
    if (start < 0 || end > len || end < start) {
        throw scheme_exception(L"substring: index out of range.");
    }
    return string2scm(wstring(str,start,end-start));
}

SchemeObject* s_number_2_string(SchemeObject* n, SchemeObject* base_s) {
    assert_arg_number_type(L"number->string", 1, n);
    int base = 10;
    if (base_s != S_UNSPECIFIED) {
        assert_arg_type(L"number->string", 2, s_integer_p, base_s);
        base = scm2int(base_s);
        if (base != 10 && base != 16 && base != 8) {
            throw scheme_exception(L"number->string invalid base: " + base_s->toString());
        }
    }
    wstring s = i_number_2_string(n, base);
    return string2scm(s);
}

SchemeObject* s_string_2_number(SchemeObject* s_string, SchemeObject* base_s) {
    assert_arg_type(L"string->number", 1, s_string_p, s_string);
    int base = 10;
    if (base_s != S_UNSPECIFIED) {
        assert_arg_type(L"string->number", 2, s_integer_p, base_s);
        base = scm2int(base_s);
    }
    wstring str = scm2string(s_string);
    return i_string_2_number(str, base);
}


SchemeObject* s_integer_2_char(SchemeObject* i) {
    assert_arg_int_type(L"integer->char", 1, i);
    int n = scm2int(i);
    if (n < 0 || (n >= 0xD800 && n < 0xE000) || n >= 0x110000) {
        throw scheme_exception(L"integer->char", L"Integer out of range.");    
    } 
    return char2scm(n);
}

SchemeObject* s_char_2_integer(SchemeObject* c) {
    assert_arg_type(L"char->integer", 1, s_char_p, c);
    return int2scm(long(scm2char(c)));
}

SchemeObject* s_string_2_list(SchemeObject* s) {
    assert_arg_type(L"string->list", 1, s_string_p, s);
    SchemeObject* result = S_EMPTY_LIST;
    SchemeObject* result_tail = S_EMPTY_LIST;
    for(int i = 0; i < s->length; i++) {
      	SchemeObject* newpair = s_cons(char2scm(s->str[i]), S_EMPTY_LIST);
    	if (result == S_EMPTY_LIST) {
    	    result = newpair;
    	    result_tail = newpair;
    	} else {
    	    i_set_cdr_e(result_tail, newpair);
    	    result_tail = newpair;
    	}
    }
    return result;
}

SchemeObject* s_list_2_string(SchemeObject* p) {
    assert_arg_type(L"list->string", 1, s_list_p, p);
    wstring result = L"";
    int i = 1;
    while (i_null_p(p) == S_FALSE) {
        assert_arg_type(L"list->string", i, s_char_p, s_car(p));
        result += scm2char(s_car(p));
        p = s_cdr(p);
        i++;
    }
    return string2scm(result);
}

SchemeObject* s_char_alphabetic_p(SchemeObject* c) {
    assert_arg_type(L"char-alphabetic?", 1, s_char_p, c);
    return bool2scm(isalpha(scm2char(c)));
}

SchemeObject* s_char_numeric_p(SchemeObject* c) {
    assert_arg_type(L"char-numeric?", 1, s_char_p, c);
    return bool2scm(isdigit(scm2char(c)));
}

SchemeObject* s_char_whitespace_p(SchemeObject* c) {
    assert_arg_type(L"char-whitespace?", 1, s_char_p, c);
    return bool2scm(isspace(scm2char(c)));
}

SchemeObject* s_char_upper_case_p(SchemeObject* c) {
    assert_arg_type(L"char-upper-case?", 1, s_char_p, c);
    return bool2scm(isupper(scm2char(c)));
}

SchemeObject* s_char_lower_case_p(SchemeObject* c) {
    assert_arg_type(L"char-lower_case?", 1, s_char_p, c);
    return bool2scm(islower(scm2char(c)));
}

SchemeObject* s_char_upcase(SchemeObject* c) {
    assert_arg_type(L"char-upcase", 1, s_char_p, c);
    return char2scm(toupper(scm2char(c)));    
}

SchemeObject* s_char_downcase(SchemeObject* c) {
    assert_arg_type(L"char-downcase", 1, s_char_p, c);
    return char2scm(tolower(scm2char(c)));    
}

SchemeObject* char_comparer(int num, SchemeStack::iterator args, int cmp1, int cmp2, const wchar_t* name, bool ci) 
{
    SchemeObject* prev = NULL;

    for(int i = 0; i < num; i++, args++) {
        SchemeObject* cur = *args;    
        assert_arg_type(name, i+1, s_char_p, cur);
        if (prev != NULL) {
            int cmp = ci ? (towupper(scm2char(prev)) - towupper(scm2char(cur))) : (scm2char(prev) - scm2char(cur));
            if (cmp > 0) cmp = 1;
            if (cmp < 0) cmp = -1;
            if (!(cmp == cmp1 || cmp == cmp2)) return S_FALSE;
        }
        prev = cur;
    }
    return S_TRUE;
}

SchemeObject* s_char_equal_p(int num, SchemeStack::iterator args) {
    return char_comparer(num, args, 0, 0, L"char=?", false);        
}

SchemeObject* s_char_less_p(int num, SchemeStack::iterator args) {
    return char_comparer(num, args, -1, -1, L"char<?", false);        
}

SchemeObject* s_char_greater_p(int num, SchemeStack::iterator args) {
    return char_comparer(num, args, 1, 1, L"char>?", false);        
}

SchemeObject* s_char_less_equal_p(int num, SchemeStack::iterator args) {
    return char_comparer(num, args, -1, 0, L"char<=?", false);        
}

SchemeObject* s_char_greater_equal_p(int num, SchemeStack::iterator args) {
    return char_comparer(num, args, 1, 0, L"char>=?", false);        
}

SchemeObject* s_char_ci_equal_p(int num, SchemeStack::iterator args) {
    return char_comparer(num, args, 0, 0, L"char-ci=?", true);
}

SchemeObject* s_char_ci_less_p(int num, SchemeStack::iterator args) {
    return char_comparer(num, args, -1, -1, L"char-ci<?", true);
}

SchemeObject* s_char_ci_greater_p(int num, SchemeStack::iterator args) {
    return char_comparer(num, args, 1, 1, L"char-ci>?", true);        
}

SchemeObject* s_char_ci_less_equal_p(int num, SchemeStack::iterator args) {
    return char_comparer(num, args, -1, 0, L"char-ci<=?", true);        
}

SchemeObject* s_char_ci_greater_equal_p(int num, SchemeStack::iterator args) {
    return char_comparer(num, args, 1, 0, L"char-ci>=?", true);
}

// Made my own comparators to avoid apparent incompabilities between strcmp in MacOS X and Linux.
int my_strcmp(wchar_t* s1, wchar_t* s2) {
    while(*s1 != L'\0' && *s2 != L'\0' && *s2 == *s1 ) { s1++; s2++; };
    if (*s1 == L'\0' && *s2 == L'\0') return 0;
    if (*s1 != L'\0' && *s2 == L'\0') return 1;
    if (*s1 == L'\0' && *s2 != L'\0') return -1;
    return *s1 < *s2 ? -1 : 1;
}

// Made my own comparators to avoid apparent incompabilities between strcasecmp in MacOS X and Linux.
int my_strcasecmp(wchar_t* s1, wchar_t* s2) {
    while(*s1 != L'\0' && *s2 != L'\0' && towupper(*s2) == towupper(*s1)) { s1++; s2++; };
    if (*s1 == L'\0' && *s2 == L'\0') return 0;
    if (*s1 != L'\0' && *s2 == L'\0') return 1;
    if (*s1 == L'\0' && *s2 != L'\0') return -1;
    return towupper(*s1) < towupper(*s2) ? -1 : 1;
}

SchemeObject* string_comparer(int num, SchemeStack::iterator args, int cmp1, int cmp2, const wchar_t* name, bool ci) 
{
    SchemeObject* prev = NULL;

    for(int i = 0; i < num; i++, args++) {
        SchemeObject* cur = *args;    
        assert_arg_type(name, i+1, s_string_p, cur);
        if (prev != NULL) {
            int c = ci ? my_strcasecmp(prev->str, cur->str) : my_strcmp(prev->str, cur->str);
            if (!(c == cmp1 || c == cmp2)) return S_FALSE;
        }
        prev = cur;
    }
    return S_TRUE;
}

SchemeObject* s_string_equal_p(int num, SchemeStack::iterator args) {
    return string_comparer(num, args, 0, 0, L"string=?", false);        
}

SchemeObject* s_string_less_p(int num, SchemeStack::iterator args) {
    return string_comparer(num, args, -1, -1, L"string<?", false);        
}

SchemeObject* s_string_greater_p(int num, SchemeStack::iterator args) {
    return string_comparer(num, args, 1, 1, L"string>?", false);        
}

SchemeObject* s_string_less_equal_p(int num, SchemeStack::iterator args) {
    return string_comparer(num, args, -1, 0, L"string<=?", false);        
}

SchemeObject* s_string_greater_equal_p(int num, SchemeStack::iterator args) {
    return string_comparer(num, args, 1, 0, L"string>=?", false);        
}

SchemeObject* s_string_ci_equal_p(int num, SchemeStack::iterator args) {
    return string_comparer(num, args, 0, 0, L"string-ci=?", true);        
}

SchemeObject* s_string_ci_less_p(int num, SchemeStack::iterator args) {
    return string_comparer(num, args, -1, -1, L"string-ci<?", true);        
}

SchemeObject* s_string_ci_greater_p(int num, SchemeStack::iterator args) {
    return string_comparer(num, args, 1, 1, L"string-ci>?", true);        
}

SchemeObject* s_string_ci_less_equal_p(int num, SchemeStack::iterator args) {
    return string_comparer(num, args, -1, 0, L"string-ci<=?", true);        
}

SchemeObject* s_string_ci_greater_equal_p(int num, SchemeStack::iterator args) {
    return string_comparer(num, args, 1, 0, L"string-ci>=?", true);        
}

SchemeObject* s_symgen() {
    wostringstream ss;
    ss << symgen_counter++;
    return SchemeObject::createSymbol((L"#G" + ss.str()).c_str());
}

SchemeObject* s_current_output_port() {
    return current_output_port;
}

SchemeObject* s_current_input_port() {
    return current_input_port;
}

SchemeObject* s_input_port_p(SchemeObject* o) {
    return o->type() == SchemeObject::INPUT_PORT ? S_TRUE : S_FALSE;
}

SchemeObject* s_output_port_p(SchemeObject* o) {
    return o->type() == SchemeObject::OUTPUT_PORT ? S_TRUE : S_FALSE;
}

SchemeObject* s_eof_object_p(SchemeObject* o) {
    return bool2scm(o->type() == SchemeObject::EOFTYPE);
}

SchemeObject* s_open_input_file(SchemeObject* s_filename) {
    assert_arg_type(L"open-input-file", 1, s_string_p, s_filename);
    wifstream* ifs = new wifstream(SchemeFilenames::toFilename(scm2string(s_filename)).c_str(), ios::in);

    try {
        ifs->imbue(locale(""));
    } catch (std::runtime_error e) {
        wcout << L"Warning: can't read system locale. Any UTF-8 data in " << scm2string(s_filename) << L" won't be read correctly." << endl;
    }

    if (ifs->fail()) {
        throw scheme_exception(L"Error opening file " + s_filename->toString());
    }
    return SchemeObject::createInputPort(ifs);
}

SchemeObject* s_open_output_file(SchemeObject* s_filename) {
    assert_arg_type(L"open-output-file", 1, s_string_p, s_filename);
    wofstream* ofs = new wofstream(SchemeFilenames::toFilename(scm2string(s_filename)).c_str(), ios::out);
    try {
        ofs->imbue(locale(""));
    } catch (std::runtime_error e) {
        wcout << L"Warning: can't read system locale. Any UTF-8 written to " << scm2string(s_filename) << L" won't work." << endl;
    }
    
    if (ofs->fail()) {
        throw scheme_exception(L"Error opening file " + s_filename->toString() + L" for writing.");
    }
    return SchemeObject::createOutputPort(ofs);
}

SchemeObject* s_close_input_port(SchemeObject* s_port) {
    assert_arg_type(L"close-input-port", 1, s_input_port_p, s_port);
    wistream* is = s_port->is;
    // Only file-streams can be closed in C++
    wifstream* ifs = static_cast<wifstream*>(is);
    if (ifs != NULL) {
       ifs->close();
    }
    return S_UNSPECIFIED;
}

SchemeObject* s_close_output_port(SchemeObject* s_port) {
    assert_arg_type(L"close-output-port", 1, s_output_port_p, s_port);
    wostream* os = s_port->os;
    // Only file-streams can be closed in C++
    wofstream* ofs = static_cast<wofstream*>(os);
    if (ofs != NULL) {
       ofs->close();
    }
    return S_UNSPECIFIED;
}

SchemeObject* s_call_with_input_file(SchemeObject* s_filename, SchemeObject* s_proc) {
    assert_arg_type(L"call-with-input-file", 1, s_string_p, s_filename);
    assert_arg_type(L"call-with-input-file", 2, s_procedure_p, s_proc);
    SchemeObject* input_port = s_open_input_file(s_filename);
    SchemeObject* result = interpreter->call_procedure_1(s_proc, input_port);
    s_close_input_port(input_port);
    return result;
}

SchemeObject* s_call_with_output_file(SchemeObject* s_filename, SchemeObject* s_proc) {
    assert_arg_type(L"call-with-output-file", 1, s_string_p, s_filename);
    assert_arg_type(L"call-with-output-file", 2, s_procedure_p, s_proc);
    SchemeObject* output_port = s_open_output_file(s_filename);
    SchemeObject* result = interpreter->call_procedure_1(s_proc, output_port);
    s_close_output_port(output_port);
    return result;
}

SchemeObject* s_with_output_to_file(SchemeObject* s_filename, SchemeObject* s_thunk) {
    assert_arg_type(L"with-output-to-file", 1, s_string_p, s_filename);
    assert_arg_type(L"with-output-to-file", 2, s_procedure_p, s_thunk);
    SchemeObject* saved_output_port = current_output_port;
    current_output_port = s_open_output_file(s_filename);
    SchemeObject* result = interpreter->call_procedure_0(s_thunk);
    s_close_output_port(current_output_port);
    current_output_port = saved_output_port;
    return result;
}

SchemeObject* s_with_input_from_file(SchemeObject* s_filename, SchemeObject* s_thunk) {
    assert_arg_type(L"with-input-from-file", 1, s_string_p, s_filename);
    assert_arg_type(L"with-input-from-file", 2, s_procedure_p, s_thunk);
    SchemeObject* saved_input_port = current_input_port;
    current_input_port = s_open_input_file(s_filename);
    SchemeObject* result = interpreter->call_procedure_0(s_thunk);
    s_close_input_port(current_input_port);
    current_input_port = saved_input_port;
    return result;
}

SchemeObject* s_read_char(SchemeObject* s_port) {
    wistream* is;
    if (s_port == S_UNSPECIFIED) {
        is = s_current_input_port()->is;
    } else {
        assert_arg_type(L"read-char", 1, s_input_port_p, s_port);
        is = s_port->is;
    }
    int c = is->get();
    if (c == -1) {
        return S_EOF;
    } else {
        return char2scm(c);
    }
}

SchemeObject* s_peek_char(SchemeObject* s_port) {
    wistream* is;
    if (s_port == S_UNSPECIFIED) {
        is = s_current_input_port()->is;
    } else {
        assert_arg_type(L"peek-char", 1, s_input_port_p, s_port);
        is = s_port->is;
    }
    int c = is->peek();
    if (c == -1) {
        return S_EOF;
    } else {
        return char2scm(c);
    }
}

SchemeObject* s_write_char(SchemeObject* s_char, SchemeObject* port) {
    assert_arg_type(L"write-char", 1, s_char_p, s_char);
    wostream* os;
    if (port == S_UNSPECIFIED) {
        os = s_current_output_port()->os;
    } else {
        assert_arg_type(L"write-char", 2, s_output_port_p, port);
        os = port->os;
    }
    (*os) << scm2char(s_char);
    return S_UNSPECIFIED;
}

SchemeObject* s_read(SchemeObject* s_port) {
    wistream* is;
    if (s_port == S_UNSPECIFIED) {
        is = s_current_input_port()->is;
    } else {
        assert_arg_type(L"read", 1, s_input_port_p, s_port);
        is = s_port->is;
    }
    return global_parser->read(is);
}


SchemeObject* s_write(SchemeObject* o, SchemeObject* port) {
    wostream* os;
    if (port == S_UNSPECIFIED) {
        os = s_current_output_port()->os;
    } else {
        assert_arg_type(L"write", 2, s_output_port_p, port);
        os = port->os;
    }
    (*os) << o->toString();
    return S_UNSPECIFIED;
}

SchemeObject* s_display(SchemeObject* o, SchemeObject* port) {
    wostream* os;
    if (port == S_UNSPECIFIED) {
        os = s_current_output_port()->os;
    } else {
        assert_arg_type(L"display", 2, s_output_port_p, port);
        os = port->os;
    }
    
    if (s_string_p(o) == S_TRUE) {
        (*os) << o->str;
    } else if (s_char_p(o) == S_TRUE) {
        (*os) << o->c;
    } else {
        (*os) << o->toString();
    }
    return S_UNSPECIFIED;
}

SchemeObject* s_newline(SchemeObject* port) {
    wostream* os;
    if (port == S_UNSPECIFIED) {
        os = s_current_output_port()->os;
    } else {
        assert_arg_type(L"write", 2, s_output_port_p, port);
        os = port->os;
    }
    (*os) << endl;        
    return S_UNSPECIFIED;
}

SchemeObject* s_environment_p(SchemeObject* o) {
    SchemeObject::ObjectType t = o->type();
    return bool2scm(t == SchemeObject::ENVIRONMENT || 
	            t == SchemeObject::SIMPLE_ENVIRONMENT);
}

SchemeObject* s_null_environment(SchemeObject* s_version) {
    assert_arg_type(L"null-environment", 1, s_integer_p, s_version);
    if (scm2int(s_version) != 5) {
        throw scheme_exception(L"Not a valid version. Only 5 is supported.");
    }
    return null_environment;
}

SchemeObject* s_scheme_report_environment(SchemeObject* s_version) {
    assert_arg_type(L"scheme-report-environment", 1, s_integer_p, s_version);
    if (scm2int(s_version) != 5) {
        throw scheme_exception(L"scheme-report-environment", L"Not a valid version. Only 5 is supported.");
    }
    return scheme_report_environment;
}

SchemeObject* s_interaction_environment(SchemeObject* s_version) {
    assert_arg_type(L"interaction-environment", 1, s_integer_p, s_version);
    if (scm2int(s_version) != 5) {
        throw scheme_exception(L"interaction-environment", L"Not a valid version. Only 5 is supported.");
    }
    return interaction_environment;
}

SchemeObject* s_eval(SchemeObject* expression, SchemeObject* s_environment) {
    assert_arg_type(L"eval", 2, s_environment_p, s_environment);
    SchemeObject* expressions = i_cons(expression, S_EMPTY_LIST);
    return interpreter->interpret(expressions, s_environment);
}

SchemeObject* s_load(SchemeObject* s_filename) {
    char original_working_dir[2048];

    assert_arg_type(L"load", 1, s_string_p, s_filename);
    wstring filename = scm2string(s_filename);

    // Change cwd to this files parent folder
    getcwd(original_working_dir, 2048);
    wstring original_cwds = SchemeFilenames::toString(string(original_working_dir));
    wstring cwd = original_cwds + L"/" + filename;
    wstring filename_clean = wstring(cwd);
    int idx = cwd.find_last_of(L'/');
    cwd.resize(idx);
    filename_clean = filename_clean.substr(idx+1, filename_clean.length());
    chdir(SchemeFilenames::toFilename(cwd).c_str());

    wifstream infile;
    infile.open(SchemeFilenames::toFilename(filename_clean).c_str(), ifstream::in);
    try {
        infile.imbue(locale(""));
    } catch (std::runtime_error e) {
        cout << "Warning: can't read system locale. UTF-8 files won't be read correctly." << endl;
    }
    
    if (infile.fail()) {
        throw scheme_exception(L"Failed loading file: " + filename);            
    }
    try {
        SchemeObject* parse_tree = global_parser->parse(&infile);
        interpreter->interpret(parse_tree, interaction_environment);
    } catch (scheme_exception e) {
        throw scheme_exception(L"In sourcefile " + filename + L": " + e.toString());    
    }
    infile.close();

    chdir(original_working_dir);

    return S_UNSPECIFIED;
}

///////////////////////////////////////////////////
// string->number
///////////////////////////////////////////////////

int extractDigit(wchar_t c, uint32_t radix) {
    int result = -1;
    result = c - L'0';
    if (radix > 10) {
	if (result < 0 || result > 9) {
	    result = 10 + (towlower(c) - L'a');
	}
    }
    
    // Check result
    if (result >= 0 && result < int(radix)) {
	return result;
    } else {
	return -1;
    }
}

char digitToChar(int d, uint32_t radix) {
    if (radix > 10 && d > 9) {
	return 'a' + (d - 10);
    } else {
	return '0' + d;
    }
}

string extractDigits(wstring s, size_t offset, uint32_t radix) {
    string result;
    uint32_t i = 0;
    while (offset + i < s.size()) {
	wchar_t digit = s[offset+i];
	int d = extractDigit(digit, radix);
	if (d == -1) {
	    return result;
	}
	result += digitToChar(d, radix);
	i++;
    }
    return result;
}

/// Returns a number object or S_FALSE in case of failure
SchemeObject* i_string_2_number(wstring s, uint32_t radix, size_t offset) {
    double sign = 1;
    if (s.size() == 0) {
	return S_FALSE;
    }
    if (s[offset] == L'#' && s.size() > offset+2) {
	wchar_t prefix = s[offset+1];
	switch(prefix) {
            case L'x' : radix = 16;
			break;
            case L'd' : radix = 10;
			break;
            case L'b' : radix = 2;
			break;
            case L'o' : radix = 8;
			break;
	    case L'e' :
            case L'i' : break;
	}
	offset += 2;
    }
    
    bool sign_found = false;
    if (s[offset] == L'-') {
        offset++;   
        sign = -1;
        sign_found = true;
    } else if (s[offset] == L'+') {
        offset++;   
        sign_found = true;
    }
    if (offset >= s.size()) {
        return S_FALSE;
    }
    
    if (s[offset] == L'i' && sign_found == true) {
        offset++;
        if (offset >= s.size()) {
            return SchemeObject::createComplexNumber(std::complex<double>(0.0,sign));        
        } else {
            return S_FALSE;        
        }
    }
    
    string digits = extractDigits(s, offset, radix);
    long t;
    if (digits.size() == 0) {
        t = 0;   
    } else {
        errno = 0;    
        t = strtol(digits.c_str(), NULL, radix);
        if (errno == ERANGE) {
            // TODO: Create a bigint        
            // throw scheme_exception(L"Number out of range");        
            return S_FALSE;
        }
    }

    offset += digits.size();
    if (offset >= s.size()) {
	// Done. No . or e
	// We have an int or a bigint
        return SchemeObject::createIntegerNumber(sign*t);
    }
    
    if (s[offset] == L'/') {
        offset++;
        string denominator = extractDigits(s, offset, radix);
        if (denominator.size() == 0) {
            return S_FALSE;        
        }
        errno = 0;
        long d = strtol(denominator.c_str(), NULL, radix);
        if (errno == ERANGE) {
            // TODO: Create a bigint        
            //throw scheme_exception(L"Number out of range");        
            return S_FALSE;
        }
        offset += denominator.size();
        if (offset >= s.size()) {
            if (d == 0) {
                return S_FALSE;
            }        
            return SchemeObject::createRationalNumber(sign*t, d);        
        } else {
            return S_FALSE;        
        }
    }
    
    
    double df = 0;
    string fraction;
    if (s[offset] == L'.' && radix == 10) {
        offset++;
        fraction = extractDigits(s, offset, 10);
        if (digits.size() == 0 && fraction.size() == 0) {
            // "." is not a number        
            return S_FALSE;        
        }
        errno = 0;    
        long f = strtol(fraction.c_str(), NULL, 10);
        if (errno == ERANGE) {
            // TODO: Clip fraction to the possible precision.        
            // throw scheme_exception(L"Number out of range"); 
            return S_FALSE;
                   
        }
        df = double(f) / pow(10.0, double(fraction.size()));
        offset += fraction.size();
    }
    if (offset >= s.size()) {
	// Done. No e
        return SchemeObject::createRealNumber(sign*(t + df));
    }
    
    long e = 0;
    if (radix == 10 && (s[offset] == L'e' || s[offset] == L's' ||
                        s[offset] == L'f' || s[offset] == L'd' || s[offset] == L'l')) {
        offset++;
        long esign = 1;
        if (s[offset] == '-') {
            esign = -1;
            offset++;
        }
        if (s[offset] == '+') {
            esign = 1;
            offset++;
        }
        string exponent = extractDigits(s, offset, 10);
        if (exponent.size() == 0) {
             return S_FALSE;
        }
        errno = 0;
        e = esign * strtol(exponent.c_str(), NULL, 10);
        if (errno == ERANGE) {
            throw scheme_exception(L"Number out of range");        
        }
        offset += exponent.size();
    }
    if (offset >= s.size()) {
        return SchemeObject::createRealNumber(sign*(t + df) * pow(10,double(e)));
    }
    
    if (s[offset] == L'i' && (digits.size() != 0 || fraction.size() != 0) && sign_found) {
        offset++;    
        if (offset >= s.size()) {
            std::complex<double> z(0, sign*(t + df) * pow(10,double(e)));    
            return SchemeObject::createComplexNumber(z);    
        } else {
            return S_FALSE;        
        }
    }
    
    if (digits.size() != 0 || fraction.size() != 0) {
        SchemeObject* imag = i_string_2_number(s, radix, offset);
        if (imag->type() == SchemeObject::COMPLEX_NUMBER) {
           SchemeObject* real = SchemeObject::createRealNumber(sign*(t + df) * pow(10,double(e)));;    
           return SchemeObject::createComplexNumber(real, imag->imag);
        }
    }
    
    return S_FALSE;
}

wstring i_number_2_string(SchemeObject* o, uint32_t radix) {
    std::wostringstream ss;
    SchemeObject::ObjectType type = o->type();
    if (type == SchemeObject::INTEGER_NUMBER) {
        ss << std::setbase(radix) << scm2int(o);
        return ss.str();
    } else if (type == SchemeObject::RATIONAL_NUMBER) {
        ss << i_number_2_string(o->numerator, radix);
        ss << L"/";
        ss << i_number_2_string(o->denominator, radix);
        return ss.str();
    } else if (type == SchemeObject::REAL_NUMBER) {
        double d = o->realValue();
        // Guile uses precision 15. We'll do the same.
        // Comparing (* 4 (atan 1)) to the real digits of pi
        // it looks like the precision of double is about 15 
        // or 16 decimal digits.     
        ss << std::setbase(radix) << std::setprecision(15) << d;
        double i;
        // Append ".0" if d contains no decimal point.
        if (::modf(d,&i) == 0.0) {
            ss << ".0";        
        }
        return ss.str();
    } else if (type == SchemeObject::COMPLEX_NUMBER) {
        double imag = o->imag->realValue();
        if (imag == 0.0) {
            return i_number_2_string(o->real, radix);        
        } else {
            ss << i_number_2_string(o->real, radix);
            if (o->imag->realValue() >= 0.0) {
                ss << L"+";        
            }
            ss << i_number_2_string(o->imag, radix);
            ss << L"i";
            return ss.str();
        }
    } else {
        throw scheme_exception(L"Not a number");
    }
}
