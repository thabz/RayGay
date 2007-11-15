

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
#include "numbers.h"

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
SchemeObject* lambda_symbol_short;
SchemeObject* quote_symbol;
SchemeObject* quasiquote_symbol;
SchemeObject* unquote_symbol;
SchemeObject* unquote_splicing_symbol;
SchemeObject* define_symbol;
SchemeObject* define_macro;
SchemeObject* set_e_symbol;
SchemeObject* define_syntax_symbol;
SchemeObject* unnamed_symbol;
SchemeObject* let_syntax_symbol;
SchemeObject* letrec_syntax_symbol;
SchemeObject* ellipsis_symbol;

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
        assign(L"interaction-environment",0,0,0, (SchemeObject* (*)()) s_interaction_environment, scheme_report_environment);


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
        assign(L"\x03BB",       SchemeObject::createInternalProcedure(L"\x03BB"), null_environment);
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
        lambda_symbol_short = SchemeObject::createSymbol(L"\x03BB");
        quote_symbol = SchemeObject::createSymbol(L"quote");
        quasiquote_symbol = SchemeObject::createSymbol(L"quasiquote");
        define_symbol = SchemeObject::createSymbol(L"define");
        define_macro = SchemeObject::createSymbol(L"define-macro");
        set_e_symbol = SchemeObject::createSymbol(L"set!");
        define_syntax_symbol = SchemeObject::createSymbol(L"define-syntax");
        let_syntax_symbol = SchemeObject::createSymbol(L"let-syntax");
        letrec_syntax_symbol = SchemeObject::createSymbol(L"letrec-syntax");
        ellipsis_symbol = SchemeObject::createSymbol(L"...");
        
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
        
        LibNumbers::bind(this, scheme_report_environment);
        
        eval(L"(define-macro (values . x) `(list ,@x))", scheme_report_environment);
        eval(L"(define-macro (call-with-values f g)  `(apply ,g (,f)))", scheme_report_environment);
        // TODO: Memoize the result of delayed value.
        // R6RS has a definition for delay that does just that - but uses syntax-rules.
        eval(L"(define-macro (delay e) `(lambda () ,e))", scheme_report_environment);
        eval(L"(define-macro (force e) `(,e))", scheme_report_environment);

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

SchemeObject* Scheme::getInteractionEnvironment() {
    return interaction_environment;
}

Interpreter* Scheme::getInterpreter() {
    return interpreter;
}

void Scheme::forceGarbageCollection() {
    Heap::getUniqueInstance()->garbageCollect(interpreter->getState()->stack);
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
        result = scm2double(a) == scm2double(b);    
    } else if (ta == SchemeObject::COMPLEX_NUMBER && tb == SchemeObject::COMPLEX_NUMBER) {
        result = scm2complex(a) == scm2complex(b);    
    } else if (ta == SchemeObject::INTEGER_NUMBER && tb == SchemeObject::INTEGER_NUMBER) {
        result = scm2int(a) == scm2int(b);    
    } else if (ta == SchemeObject::RATIONAL_NUMBER && tb == SchemeObject::RATIONAL_NUMBER) {
        result = scm2rational(a) == scm2rational(b);  
    } else if (ta == SchemeObject::SYMBOL && tb == SchemeObject::SYMBOL) {
        result = a == b;
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
    Interpreter::State* state = interpreter->getState();
    assert_arg_type(L"proc", 1, s_procedure_p, s_proc);
    SchemeObject* s_escape = SchemeObject::createContinuation();
    size_t stack_size = state->stack.size();
    interpreter->getState()->stack.push_back(s_escape);
    s_escape->jmpbuf = (::jmp_buf *)::malloc(sizeof(::jmp_buf));
    int kk = setjmp(*(s_escape->jmpbuf));
    if (kk == 0) {
        SchemeObject* result = interpreter->call_procedure_1(s_proc, s_escape);
        // s_proc didn't call the escape-continuation
        state->stack.resize(stack_size);
        return result;
    }
    state->stack.resize(stack_size);
    return s_escape->result;
}

// TODO: Gør denne til en intern function (igen) med tail-optimization i kaldet til proc.
// TODO: Det er måske nemmere at skrive denne som en macro, ala (define-macro (apply proc args) `(,proc ,@args))
// args is a list (arg1 arg2 ... argn). argn must be a list. proc is called with the arguments
// (append (list arg1 arg2 ...) argn)
SchemeObject* s_apply(int num, SchemeStack::iterator args) {
    Interpreter::State* state = interpreter->getState();
        
    SchemeObject* proc = *args++;
    num--;
    assert_arg_type(L"apply", 1, s_procedure_p, proc);

    state->stack.push_back(S_EMPTY_LIST);
    SchemeObject*& collected = state->stack.back();
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
    state->stack.pop_back();
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
               interpreter->getState()->stack.push_back(result);
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
        interpreter->getState()->stack.pop_back();
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
