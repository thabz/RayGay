
#include "scheme.h"
#include <fstream>
#include <iomanip>
#include <cmath>
#include <cctype>

#include "lexer.h"
#include "parser.h"
#include "interpreter.h"

scheme_exception::scheme_exception(string s) : str(s), procname(NULL) {
}

scheme_exception::scheme_exception(char* procname, string error) : str(error), procname(procname) {
}

string scheme_exception::toString() {
    if (procname != NULL) {
        return "In procedure " + string(procname) + ": " + str;            
    } else {
        return str;    
    }     
}

unsigned long symgen_counter = 10000;

SchemeObject* S_TRUE = SchemeObject::createBool(true);
SchemeObject* S_FALSE = SchemeObject::createBool(false);
SchemeObject* S_EOF = SchemeObject::createEOF();
SchemeObject* S_ZERO = SchemeObject::createNumber(0);
SchemeObject* S_ONE = SchemeObject::createNumber(1);
SchemeObject* S_TWO = SchemeObject::createNumber(2);
SchemeObject* S_THREE = SchemeObject::createNumber(3);
SchemeObject* S_FOUR = SchemeObject::createNumber(4);
SchemeObject* S_FIVE = SchemeObject::createNumber(5);
SchemeObject* S_SIX = SchemeObject::createNumber(6);
SchemeObject* S_SEVEN = SchemeObject::createNumber(7);
SchemeObject* S_EIGHT = SchemeObject::createNumber(8);
SchemeObject* S_NINE = SchemeObject::createNumber(9);
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

Interpreter* interpreter;

//#define R5RS_STRICT

#ifdef R5RS_STRICT 
//SchemeObject* null_environment = SchemeObject::createEnvironment(NULL);
//SchemeObject* scheme_report_environment = SchemeObject::createEnvironment(null_environment);
//SchemeObject* interaction_environment = SchemeObject::createEnvironment(scheme_report_environment);
#else 
SchemeObject* null_environment = SchemeObject::createEnvironment(NULL);
SchemeObject* scheme_report_environment = null_environment;
SchemeObject* interaction_environment = null_environment;
#endif

// Global parser used by s_read()
Parser* global_parser = new Parser();

bool globals_init = false;

Scheme::Scheme() {
    if (!globals_init) {
        globals_init = true;

    	assign("map"                   ,1,0,1, (SchemeObject* (*)()) s_map, scheme_report_environment);
    	assign("for-each"              ,1,0,1, (SchemeObject* (*)()) s_for_each, scheme_report_environment);
    	assign("equal?"                ,2,0,0, (SchemeObject* (*)()) s_equal_p, scheme_report_environment);
    	assign("eq?"                   ,2,0,0, (SchemeObject* (*)()) s_eq_p, scheme_report_environment);
    	assign("eqv?"                  ,2,0,0, (SchemeObject* (*)()) s_eqv_p, scheme_report_environment);
    	assign("boolean?"              ,1,0,0, (SchemeObject* (*)()) s_boolean_p, scheme_report_environment);
    	assign("pair?"                 ,1,0,0, (SchemeObject* (*)()) s_pair_p, scheme_report_environment);
    	assign("symbol?"               ,1,0,0, (SchemeObject* (*)()) s_symbol_p, scheme_report_environment);
    	assign("char?"                 ,1,0,0, (SchemeObject* (*)()) s_char_p, scheme_report_environment);
    	assign("list?"                 ,1,0,0, (SchemeObject* (*)()) s_list_p, scheme_report_environment);
    	assign("string?"               ,1,0,0, (SchemeObject* (*)()) s_string_p, scheme_report_environment);
    	assign("procedure?"            ,1,0,0, (SchemeObject* (*)()) s_procedure_p, scheme_report_environment);
    	assign("number?"               ,1,0,0, (SchemeObject* (*)()) s_number_p, scheme_report_environment);
    	assign("integer?"              ,1,0,0, (SchemeObject* (*)()) s_integer_p, scheme_report_environment);
    	assign("complex?"              ,1,0,0, (SchemeObject* (*)()) s_complex_p, scheme_report_environment);
    	assign("real?"                 ,1,0,0, (SchemeObject* (*)()) s_real_p, scheme_report_environment);
    	assign("rational?"             ,1,0,0, (SchemeObject* (*)()) s_rational_p, scheme_report_environment);
    	assign("exact?"                ,1,0,0, (SchemeObject* (*)()) s_exact_p, scheme_report_environment);
    	assign("inexact?"              ,1,0,0, (SchemeObject* (*)()) s_inexact_p, scheme_report_environment);
    	assign("vector?"               ,1,0,0, (SchemeObject* (*)()) s_vector_p, scheme_report_environment);
    	assign("null?"                 ,1,0,0, (SchemeObject* (*)()) s_null_p, scheme_report_environment);
    	assign("car"                   ,1,0,0, (SchemeObject* (*)()) s_car, scheme_report_environment);
    	assign("cdr"                   ,1,0,0, (SchemeObject* (*)()) s_cdr, scheme_report_environment);
    	assign("cadr"                  ,1,0,0, (SchemeObject* (*)()) s_cadr, scheme_report_environment);
    	assign("cdar"                  ,1,0,0, (SchemeObject* (*)()) s_cdar, scheme_report_environment);
    	assign("cddr"                  ,1,0,0, (SchemeObject* (*)()) s_cddr, scheme_report_environment);
    	assign("caaar"                 ,1,0,0, (SchemeObject* (*)()) s_caaar, scheme_report_environment);
    	assign("caadr"                 ,1,0,0, (SchemeObject* (*)()) s_caadr, scheme_report_environment);
    	assign("cadar"                 ,1,0,0, (SchemeObject* (*)()) s_cadar, scheme_report_environment);
    	assign("caddr"                 ,1,0,0, (SchemeObject* (*)()) s_caddr, scheme_report_environment);
    	assign("cdaar"                 ,1,0,0, (SchemeObject* (*)()) s_cdaar, scheme_report_environment);
    	assign("cdadr"                 ,1,0,0, (SchemeObject* (*)()) s_cdadr, scheme_report_environment);
    	assign("cddar"                 ,1,0,0, (SchemeObject* (*)()) s_cddar, scheme_report_environment);
    	assign("cdddr"                 ,1,0,0, (SchemeObject* (*)()) s_cdddr, scheme_report_environment);
    	assign("caaaar"                ,1,0,0, (SchemeObject* (*)()) s_caaaar, scheme_report_environment);
    	assign("caaadr"                ,1,0,0, (SchemeObject* (*)()) s_caaadr, scheme_report_environment);
    	assign("caadar"                ,1,0,0, (SchemeObject* (*)()) s_caadar, scheme_report_environment);
    	assign("caaddr"                ,1,0,0, (SchemeObject* (*)()) s_caaddr, scheme_report_environment);
    	assign("cadaar"                ,1,0,0, (SchemeObject* (*)()) s_cadaar, scheme_report_environment);
    	assign("cadadr"                ,1,0,0, (SchemeObject* (*)()) s_cadadr, scheme_report_environment);
    	assign("caddar"                ,1,0,0, (SchemeObject* (*)()) s_caddar, scheme_report_environment);
    	assign("cadddr"                ,1,0,0, (SchemeObject* (*)()) s_cadddr, scheme_report_environment);                                       
    	assign("cdaaar"                ,1,0,0, (SchemeObject* (*)()) s_cdaaar, scheme_report_environment);
    	assign("cdaadr"                ,1,0,0, (SchemeObject* (*)()) s_cdaadr, scheme_report_environment);
    	assign("cdadar"                ,1,0,0, (SchemeObject* (*)()) s_cdadar, scheme_report_environment);
    	assign("cdaddr"                ,1,0,0, (SchemeObject* (*)()) s_cdaddr, scheme_report_environment);
    	assign("cddaar"                ,1,0,0, (SchemeObject* (*)()) s_cddaar, scheme_report_environment);
    	assign("cddadr"                ,1,0,0, (SchemeObject* (*)()) s_cddadr, scheme_report_environment);
    	assign("cdddar"                ,1,0,0, (SchemeObject* (*)()) s_cdddar, scheme_report_environment);
    	assign("cddddr"                ,1,0,0, (SchemeObject* (*)()) s_cddddr, scheme_report_environment);
    	assign("list"                  ,0,0,1, (SchemeObject* (*)()) s_list, scheme_report_environment);
    	assign("list-tail"             ,2,0,0, (SchemeObject* (*)()) s_list_tail, scheme_report_environment);
    	assign("list-ref"              ,2,0,0, (SchemeObject* (*)()) s_list_ref, scheme_report_environment);
    	assign("set-car!"              ,2,0,0, (SchemeObject* (*)()) s_set_car_e, scheme_report_environment);
    	assign("set-cdr!"              ,2,0,0, (SchemeObject* (*)()) s_set_cdr_e, scheme_report_environment);
    	assign("assoc"                 ,2,0,0, (SchemeObject* (*)()) s_assoc, scheme_report_environment);
    	assign("assq"                  ,2,0,0, (SchemeObject* (*)()) s_assq, scheme_report_environment);
    	assign("assv"                  ,2,0,0, (SchemeObject* (*)()) s_assv, scheme_report_environment);
    	assign("append"                ,0,0,1, (SchemeObject* (*)()) s_append, scheme_report_environment);
    	assign("member"                ,2,0,0, (SchemeObject* (*)()) s_member, scheme_report_environment);
    	assign("memq"                  ,2,0,0, (SchemeObject* (*)()) s_memq, scheme_report_environment);
    	assign("memv"                  ,2,0,0, (SchemeObject* (*)()) s_memv, scheme_report_environment);
    	assign("reverse"               ,1,0,0, (SchemeObject* (*)()) s_reverse, scheme_report_environment);
    	assign("length"                ,1,0,0, (SchemeObject* (*)()) s_length, scheme_report_environment);
    	assign("cons"                  ,2,0,0, (SchemeObject* (*)()) s_cons, scheme_report_environment);
    	assign("display"               ,1,1,0, (SchemeObject* (*)()) s_display, scheme_report_environment);
    	assign("write"                 ,1,1,0, (SchemeObject* (*)()) s_write, scheme_report_environment);
    	assign("newline"               ,0,1,0, (SchemeObject* (*)()) s_newline, scheme_report_environment);
    	assign("<"                     ,0,0,1, (SchemeObject* (*)()) s_less, scheme_report_environment);
    	assign(">"                     ,0,0,1, (SchemeObject* (*)()) s_greater, scheme_report_environment);
    	assign("<="                    ,0,0,1, (SchemeObject* (*)()) s_less_equal, scheme_report_environment);
    	assign(">="                    ,0,0,1, (SchemeObject* (*)()) s_greater_equal, scheme_report_environment);
    	assign("="                     ,0,0,1, (SchemeObject* (*)()) s_equal, scheme_report_environment);
    	assign("+"                     ,0,0,1, (SchemeObject* (*)()) s_plus, scheme_report_environment);
    	assign("-"                     ,1,0,1, (SchemeObject* (*)()) s_minus, scheme_report_environment);
    	assign("*"                     ,0,0,1, (SchemeObject* (*)()) s_mult, scheme_report_environment);
    	assign("/"                     ,1,0,1, (SchemeObject* (*)()) s_divide, scheme_report_environment);
    	assign("abs"                   ,1,0,0, (SchemeObject* (*)()) s_abs, scheme_report_environment);
    	assign("tan"                   ,1,0,0, (SchemeObject* (*)()) s_tan, scheme_report_environment);
    	assign("atan"                  ,1,1,0, (SchemeObject* (*)()) s_atan, scheme_report_environment);
    	assign("sin"                   ,1,0,0, (SchemeObject* (*)()) s_sin, scheme_report_environment);
    	assign("asin"                  ,1,0,0, (SchemeObject* (*)()) s_asin, scheme_report_environment);
    	assign("cos"                   ,1,0,0, (SchemeObject* (*)()) s_cos, scheme_report_environment);
    	assign("acos"                  ,1,0,0, (SchemeObject* (*)()) s_acos, scheme_report_environment);
    	assign("sqrt"                  ,1,0,0, (SchemeObject* (*)()) s_sqrt, scheme_report_environment);
    	assign("log"                   ,1,0,0, (SchemeObject* (*)()) s_log, scheme_report_environment);
    	assign("exp"                   ,1,0,0, (SchemeObject* (*)()) s_exp, scheme_report_environment);
    	assign("expt"                  ,2,0,0, (SchemeObject* (*)()) s_expt, scheme_report_environment);
    	assign("round"                 ,1,0,0, (SchemeObject* (*)()) s_round, scheme_report_environment);
    	assign("ceiling"               ,1,0,0, (SchemeObject* (*)()) s_ceiling, scheme_report_environment);
    	assign("floor"                 ,1,0,0, (SchemeObject* (*)()) s_floor, scheme_report_environment);
    	assign("truncate"              ,1,0,0, (SchemeObject* (*)()) s_truncate, scheme_report_environment);
    	assign("quotient"              ,2,0,0, (SchemeObject* (*)()) s_quotient, scheme_report_environment);
    	assign("remainder"             ,2,0,0, (SchemeObject* (*)()) s_remainder, scheme_report_environment);
    	assign("modulo"                ,2,0,0, (SchemeObject* (*)()) s_modulo, scheme_report_environment);
    	assign("min"                   ,1,0,1, (SchemeObject* (*)()) s_min, scheme_report_environment);
    	assign("max"                   ,1,0,1, (SchemeObject* (*)()) s_max, scheme_report_environment);
    	assign("gcd"                   ,0,0,1, (SchemeObject* (*)()) s_gcd, scheme_report_environment);
    	assign("lcm"                   ,0,0,1, (SchemeObject* (*)()) s_lcm, scheme_report_environment);
    	assign("even?"                 ,1,0,1, (SchemeObject* (*)()) s_even_p, scheme_report_environment);
    	assign("odd?"                  ,1,0,1, (SchemeObject* (*)()) s_odd_p, scheme_report_environment);
    	assign("zero?"                 ,1,0,0, (SchemeObject* (*)()) s_zero_p, scheme_report_environment);
    	assign("negative?"             ,1,0,0, (SchemeObject* (*)()) s_negative_p, scheme_report_environment);
    	assign("positive?"             ,1,0,0, (SchemeObject* (*)()) s_positive_p, scheme_report_environment);
    	assign("not"                   ,1,0,0, (SchemeObject* (*)()) s_not, scheme_report_environment);
    	assign("make-vector"           ,1,1,0, (SchemeObject* (*)()) s_make_vector, scheme_report_environment);
    	assign("vector"                ,0,0,1, (SchemeObject* (*)()) s_vector, scheme_report_environment);
    	assign("vector-length"         ,1,0,0, (SchemeObject* (*)()) s_vector_length, scheme_report_environment);
    	assign("vector-ref"            ,2,0,0, (SchemeObject* (*)()) s_vector_ref, scheme_report_environment);
    	assign("vector-set!"           ,3,0,0, (SchemeObject* (*)()) s_vector_set_e, scheme_report_environment);
    	assign("vector-fill!"          ,2,0,0, (SchemeObject* (*)()) s_vector_fill_e, scheme_report_environment);
    	assign("list->vector"          ,1,0,0, (SchemeObject* (*)()) s_list_2_vector, scheme_report_environment);
    	assign("vector->list"          ,1,0,0, (SchemeObject* (*)()) s_vector_2_list, scheme_report_environment);
    	assign("make-string"           ,1,1,0, (SchemeObject* (*)()) s_make_string, scheme_report_environment);
    	assign("string"                ,0,0,1, (SchemeObject* (*)()) s_string, scheme_report_environment);
    	assign("string-length"         ,1,0,0, (SchemeObject* (*)()) s_string_length, scheme_report_environment);
    	assign("string-ref"            ,2,0,0, (SchemeObject* (*)()) s_string_ref, scheme_report_environment);
    	assign("string-set!"           ,3,0,0, (SchemeObject* (*)()) s_string_set_e, scheme_report_environment);
    	assign("string-append"         ,0,0,1, (SchemeObject* (*)()) s_string_append, scheme_report_environment);
    	assign("string-copy"           ,1,0,0, (SchemeObject* (*)()) s_string_copy, scheme_report_environment);
    	assign("substring"             ,3,0,0, (SchemeObject* (*)()) s_substring, scheme_report_environment);
    	assign("symbol->string"        ,1,0,0, (SchemeObject* (*)()) s_symbol_2_string, scheme_report_environment);
    	assign("string->symbol"        ,1,0,0, (SchemeObject* (*)()) s_string_2_symbol, scheme_report_environment);
    	assign("char->integer"         ,1,0,0, (SchemeObject* (*)()) s_char_2_integer, scheme_report_environment);
    	assign("integer->char"         ,1,0,0, (SchemeObject* (*)()) s_integer_2_char, scheme_report_environment);
    	assign("number->string"        ,1,1,0, (SchemeObject* (*)()) s_number_2_string, scheme_report_environment);
    	assign("string->number"        ,1,1,0, (SchemeObject* (*)()) s_string_2_number, scheme_report_environment);
    	assign("list->string"          ,1,0,0, (SchemeObject* (*)()) s_list_2_string, scheme_report_environment);
    	assign("string->list"          ,1,0,0, (SchemeObject* (*)()) s_string_2_list, scheme_report_environment);
    	assign("char-downcase"         ,1,0,0, (SchemeObject* (*)()) s_char_downcase, scheme_report_environment);
    	assign("char-upcase"           ,1,0,0, (SchemeObject* (*)()) s_char_upcase, scheme_report_environment);
    	assign("char-alphabetic?"      ,1,0,0, (SchemeObject* (*)()) s_char_alphabetic_p, scheme_report_environment);
    	assign("char-numeric?"         ,1,0,0, (SchemeObject* (*)()) s_char_numeric_p, scheme_report_environment);
    	assign("char-whitespace?"      ,1,0,0, (SchemeObject* (*)()) s_char_whitespace_p, scheme_report_environment);
    	assign("char-upper-case?"      ,1,0,0, (SchemeObject* (*)()) s_char_upper_case_p, scheme_report_environment);
    	assign("char-lower-case?"      ,1,0,0, (SchemeObject* (*)()) s_char_lower_case_p, scheme_report_environment);
    	assign("symgen"                ,0,0,0, (SchemeObject* (*)()) s_symgen, scheme_report_environment);

    	assign("current-input-port"    ,0,0,0, (SchemeObject* (*)()) s_current_input_port, scheme_report_environment);
    	assign("current-output-port"   ,0,0,0, (SchemeObject* (*)()) s_current_output_port, scheme_report_environment);
    	assign("input-port?"           ,1,0,0, (SchemeObject* (*)()) s_input_port_p, scheme_report_environment);
    	assign("output-port?"          ,1,0,0, (SchemeObject* (*)()) s_output_port_p, scheme_report_environment);
    	assign("eof-object?"           ,1,0,0, (SchemeObject* (*)()) s_eof_object_p, scheme_report_environment);
    	assign("call-with-input-file"  ,2,0,0, (SchemeObject* (*)()) s_call_with_input_file, scheme_report_environment);
    	assign("call-with-output-file" ,2,0,0, (SchemeObject* (*)()) s_call_with_output_file, scheme_report_environment);
    	assign("with-input-from-file"  ,2,0,0, (SchemeObject* (*)()) s_with_input_from_file, scheme_report_environment);
    	assign("with-output-to-file"   ,2,0,0, (SchemeObject* (*)()) s_with_output_to_file, scheme_report_environment);
    	assign("open-input-file"       ,1,0,0, (SchemeObject* (*)()) s_open_input_file, scheme_report_environment);
    	assign("open-output-file"      ,1,0,0, (SchemeObject* (*)()) s_open_output_file, scheme_report_environment);
    	assign("close-input-port"      ,1,0,0, (SchemeObject* (*)()) s_close_input_port, scheme_report_environment);
    	assign("close-output-port"     ,1,0,0, (SchemeObject* (*)()) s_close_output_port, scheme_report_environment);
    	assign("read-char"             ,0,1,0, (SchemeObject* (*)()) s_read_char, scheme_report_environment);
    	assign("peek-char"             ,0,1,0, (SchemeObject* (*)()) s_peek_char, scheme_report_environment);
    	assign("write-char"            ,1,1,0, (SchemeObject* (*)()) s_write_char, scheme_report_environment);
    	assign("read"                  ,0,1,0, (SchemeObject* (*)()) s_read, scheme_report_environment);

    	assign("apply"                 ,1,0,1, (SchemeObject* (*)()) s_apply, scheme_report_environment);
    	assign("call-with-current-continuation" ,1,0,0, (SchemeObject* (*)()) s_call_cc, scheme_report_environment);
    	assign("call/cc"               ,1,0,0, (SchemeObject* (*)()) s_call_cc, scheme_report_environment);
        assign("eval"                  ,2,0,0, (SchemeObject* (*)()) s_eval, scheme_report_environment);
        assign("scheme-report-environment",1,0,0, (SchemeObject* (*)()) s_scheme_report_environment, scheme_report_environment);
        assign("null-environment"      ,1,0,0, (SchemeObject* (*)()) s_null_environment, scheme_report_environment);
        assign("interaction-environment",1,0,0, (SchemeObject* (*)()) s_interaction_environment, scheme_report_environment);


        //assign("apply", new SchemeInternalProcedure("apply"));
        assign("if",           SchemeObject::createInternalProcedure("if"), null_environment);
        assign("cond",         SchemeObject::createInternalProcedure("cond"), null_environment);
        assign("case",         SchemeObject::createInternalProcedure("case"), null_environment);
        assign("do",           SchemeObject::createInternalProcedure("do"), null_environment);
        assign("let",          SchemeObject::createInternalProcedure("let"), null_environment);
        assign("let*",         SchemeObject::createInternalProcedure("let*"), null_environment);
        assign("letrec",       SchemeObject::createInternalProcedure("letrec"), null_environment);
        assign("begin",        SchemeObject::createInternalProcedure("begin"), null_environment);
        assign("and",          SchemeObject::createInternalProcedure("and"), null_environment);
        assign("or",           SchemeObject::createInternalProcedure("or"), null_environment);
        assign("lambda",       SchemeObject::createInternalProcedure("lambda"), null_environment);
        assign("quote",        SchemeObject::createInternalProcedure("quote"), null_environment);
        assign("quasiquote",   SchemeObject::createInternalProcedure("quasiquote"), null_environment);
        assign("define",       SchemeObject::createInternalProcedure("define"), null_environment);
        assign("define-macro", SchemeObject::createInternalProcedure("define-macro"), null_environment);
        assign("set!",         SchemeObject::createInternalProcedure("set!"), null_environment);

        current_input_port = SchemeObject::createInputPort(&cin);
        current_output_port = SchemeObject::createOutputPort(&cout);

        else_symbol = SchemeObject::createSymbol("else");
        ergo_symbol = SchemeObject::createSymbol("=>");
        unquote_symbol = SchemeObject::createSymbol("unquote");
        unquote_splicing_symbol = SchemeObject::createSymbol("unquote-splicing");
        unnamed_symbol = SchemeObject::createSymbol("#<unnamed>");

        if_symbol = SchemeObject::createSymbol("if");
        apply_symbol = SchemeObject::createSymbol("apply");
        cond_symbol = SchemeObject::createSymbol("cond");
        case_symbol = SchemeObject::createSymbol("case");
        do_symbol = SchemeObject::createSymbol("do");
        let_symbol = SchemeObject::createSymbol("let");
        letstar_symbol = SchemeObject::createSymbol("let*");
        letrec_symbol = SchemeObject::createSymbol("letrec");
        begin_symbol = SchemeObject::createSymbol("begin");
        and_symbol = SchemeObject::createSymbol("and");
        or_symbol = SchemeObject::createSymbol("or");
        lambda_symbol = SchemeObject::createSymbol("lambda");
        quote_symbol = SchemeObject::createSymbol("quote");
        quasiquote_symbol = SchemeObject::createSymbol("quasiquote");
        define_symbol = SchemeObject::createSymbol("define");
        define_macro = SchemeObject::createSymbol("define-macro");
        set_e_symbol = SchemeObject::createSymbol("set!");
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
    }
    
    ifstream infile;
    infile.open("init.scm", ifstream::in);
    eval(&infile);
    infile.close();
}

SchemeObject* Scheme::eval(istream* is) {
    Parser* parser = new Parser();
    SchemeObject* parse_tree = parser->parse(is);
    interpreter = new Interpreter(parse_tree, interaction_environment);
    return interpreter->interpret();
}

SchemeObject* Scheme::eval(string data) {
    istream* is = new istringstream(data);
    SchemeObject* result = eval(is);
    delete is;
    return result;
};

void Scheme::assign(string variable, int req, int opt, int rst, SchemeObject* (*fn)(), SchemeObject* envt = interaction_environment) {
    SchemeObject* name = SchemeObject::createSymbol(variable.c_str());
    SchemeObject* proc = SchemeObject::createBuiltinProcedure(name, req, opt, rst, fn);
    envt->defineBinding(name, proc);
}

void Scheme::assign(string variable, SchemeObject* value, SchemeObject* envt = interaction_environment) {
    SchemeObject* name = SchemeObject::createSymbol(variable.c_str());
    envt->defineBinding(name, value);
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
    return a->toString() == b->toString() ? S_TRUE : S_FALSE; 
}

SchemeObject* s_eqv_p(SchemeObject* a, SchemeObject* b) {
    if (i_number_p(a) == S_TRUE && i_number_p(b) == S_TRUE) {
        return bool2scm(scm2double(a) == scm2double(b));
    } else if (i_char_p(a) == S_TRUE && i_char_p(b) == S_TRUE) {
        return bool2scm(scm2char(a) == scm2char(b));
    } else {
        return bool2scm(a == b);
    }
}

SchemeObject* s_eq_p(SchemeObject* a, SchemeObject* b) {
    return s_eqv_p(a,b); 
}

// (boolean? b)
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



// TODO: Gør denne til en intern function med tail-optimization
SchemeObject* s_call_cc(SchemeObject* s_proc) {
    assert_arg_type("proc", 1, s_procedure_p, s_proc);
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
SchemeObject* s_apply(SchemeObject* proc, SchemeObject* args) {
    assert_arg_type("apply", 1, s_procedure_p, proc);

    stack.push_back(S_EMPTY_LIST);
    SchemeObject*& collected = stack.back();
    SchemeObject* prev = NULL;
    int i = 0;
    while (args != S_EMPTY_LIST) {
        i++;
        SchemeObject* arg = s_car(args);
        if (s_pair_p(arg) == S_TRUE || arg == S_EMPTY_LIST) {
            if (s_cdr(args) == S_EMPTY_LIST) {
                // arg is a list and last argument
                if (collected == S_EMPTY_LIST) {
                    collected = arg;
                } else {
                    s_set_cdr_e(prev, arg);
                }
            } else {
                throw scheme_exception("Illegal argument");
            }
        } else {
            if (collected == S_EMPTY_LIST) {
                collected = s_cons(arg, S_EMPTY_LIST);
                prev = collected;
            } else {
                SchemeObject* tmp = s_cons(arg,S_EMPTY_LIST);
                s_set_cdr_e(prev, tmp);
                prev = tmp;
            }
        }
        args = s_cdr(args);
    }
    stack.pop_back();
    return interpreter->call_procedure_n(proc,collected);
}

SchemeObject* s_map(SchemeObject* proc, SchemeObject* lists) {
    assert_arg_type("map", 1, s_procedure_p, proc);
    SchemeObject* lists_ptr = lists;
    int i = 2;
    while (lists_ptr != S_EMPTY_LIST) {
        assert_non_atom_type("map", i++, i_car(lists_ptr));
        lists_ptr = i_cdr(lists_ptr);
    }
    
    SchemeObject* result = S_EMPTY_LIST;
    SchemeObject* prev = S_EMPTY_LIST;

    // Vi skralder af lists i hvert gennemløb. Så ((1 2 3)(10 20 30)) bliver til ((2 3)(20 30)) og til sidst ((3)(30))
    while (s_car(lists) != S_EMPTY_LIST) {
        // Collect args
        SchemeObject* collection = S_EMPTY_LIST;
        SchemeObject* prev_col = S_EMPTY_LIST;
        lists_ptr = lists;
        while (lists_ptr != S_EMPTY_LIST) {
            if (s_car(lists_ptr) == S_EMPTY_LIST) {
                throw scheme_exception("Argument lists not equals length.");
            }
            SchemeObject* arg = s_car(s_car(lists_ptr));
            s_set_car_e(lists_ptr,s_cdr(s_car(lists_ptr)));
            if (collection == S_EMPTY_LIST) {
                collection = s_cons(arg,S_EMPTY_LIST);
                prev_col = collection;
            } else {
                SchemeObject* tmp = s_cons(arg,S_EMPTY_LIST);
                prev_col->cdr = tmp;
                prev_col = tmp;
                
            }
            lists_ptr = s_cdr(lists_ptr);
        }
        
        SchemeObject* result_item = interpreter->call_procedure_n(proc, collection);
        
        if (result == S_EMPTY_LIST) {
            result = s_cons(result_item, S_EMPTY_LIST);
            prev = result;
            stack.push_back(result);
        } else {
            SchemeObject* tmp = s_cons(result_item, S_EMPTY_LIST);
            s_set_cdr_e(prev, tmp);
            prev = tmp;
        }
    }
    // Tjek at argumentlisterne var lige lange
    while (lists != S_EMPTY_LIST) {
        if (s_car(lists) != S_EMPTY_LIST) {
            throw scheme_exception("Argument lists not equals length.");
        }
        lists = s_cdr(lists);
    }
    if (result != S_EMPTY_LIST) {
        stack.pop_back();
    }
    return result;    
}

SchemeObject* s_for_each(SchemeObject* proc, SchemeObject* lists) {
    s_map(proc, lists);
    return S_UNSPECIFIED;
}


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
    assert_arg_positive_int("list-tail", 2, k);
    int i = scm2int(k);
    while (i-- > 0) {
        if (l == S_EMPTY_LIST) {
            throw scheme_exception("Index out of range: " + k->toString());
        }
        if (s_pair_p(l) == S_FALSE) {
            throw scheme_exception("Not a proper list");
        }
        l = s_cdr(l);
    }
    return l;
}

SchemeObject* s_list_ref(SchemeObject* l, SchemeObject* k) {
    SchemeObject* p = s_list_tail(l, k);
    if (p == S_EMPTY_LIST) {
        throw scheme_exception("Index out of range: " + k->toString());
    }
    return s_car(p);
}

SchemeObject* assoc_helper(SchemeObject* (comparator)(SchemeObject*,SchemeObject*), SchemeObject* obj, SchemeObject* alist) {
    while (alist != S_EMPTY_LIST) {
        if (s_pair_p(s_car(alist)) == S_FALSE) {
            throw scheme_exception("Illegal argument");
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

// (integer? p)
SchemeObject* s_integer_p(SchemeObject* p) {
    double i;
    if (p->type() != SchemeObject::NUMBER) {
        return S_FALSE;
    }
    return ::modf(scm2double(p),&i) == 0.0 ? S_TRUE : S_FALSE;
}

SchemeObject* s_complex_p(SchemeObject* n) {
    return S_TRUE;
}

SchemeObject* s_rational_p(SchemeObject* n) {
    return S_TRUE;
}

SchemeObject* s_real_p(SchemeObject* n) {
    return S_TRUE;
}

SchemeObject* s_exact_p(SchemeObject* n) {
    assert_arg_type("exact?", 1, s_number_p, n);
    return s_integer_p(n);
}

SchemeObject* s_inexact_p(SchemeObject* n) {
    assert_arg_type("inexact?", 1, s_number_p, n);
    return s_exact_p(n) == S_TRUE ? S_FALSE : S_TRUE;
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
    assert_arg_pair_type("car", 1, o);
    return i_car(o);
}

SchemeObject* s_cdr(SchemeObject* o) {
    assert_arg_pair_type("cdr", 1, o);
    return i_cdr(o);
}

SchemeObject* s_cxr(SchemeObject* o, char* x) {
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

SchemeObject* s_list(SchemeObject* args) {
    return args;
}

SchemeObject* s_reverse(SchemeObject* o) 
{
    SchemeObject* result = S_EMPTY_LIST;

    while (o != S_EMPTY_LIST) {
        assert_arg_type("reverse", 1, s_pair_p, o);
		result = i_cons(i_car(o), result);
		o = s_cdr(o);
	}
	return result;  
}

SchemeObject* s_length(SchemeObject* p) {
    int length = 0;
    while (p != S_EMPTY_LIST) {
        assert_arg_pair_type("length", 1, p);
        length++;
        p = i_cdr(p);
    }
    return int2scm(length);
}

SchemeObject* s_set_car_e(SchemeObject* p, SchemeObject* o) {
    assert_arg_pair_type("set-car!", 1, p);
    assert_arg_not_immutable("set-car!", 1, p);
    i_set_car_e(p, o);
    return S_UNSPECIFIED;
}

SchemeObject* s_set_cdr_e(SchemeObject* p, SchemeObject* o) {
    assert_arg_pair_type("set-cdr!", 1, p);
    assert_arg_not_immutable("set-cdr!", 1, p);
    i_set_cdr_e(p, o);
    return S_UNSPECIFIED;
}

SchemeObject* s_append(SchemeObject* p) {
    SchemeObject* result = S_EMPTY_LIST;
    SchemeObject* result_tail = NULL;
    
    if (p == S_EMPTY_LIST) {
        return S_EMPTY_LIST;
    }
    
    if (s_cdr(p) == S_EMPTY_LIST) {
        return s_car(p);
    }
    int i = 1;
    while (s_cdr(p) != S_EMPTY_LIST) {
        SchemeObject* pp = s_car(p);
        // Skip empty lists
        if (pp != S_EMPTY_LIST) {
            assert_arg_type("append", i, s_pair_p, pp);

    	    while (pp != S_EMPTY_LIST) {

                assert_arg_type("append", i, s_pair_p, pp);

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
        i++;
        p = s_cdr(p);
    }

    // Append  final arg
    if (result != S_EMPTY_LIST) {
        i_set_cdr_e(result_tail, s_car(p));
    } else {
        result = s_car(p);
    }
    return result;
}

SchemeObject* s_plus(SchemeObject* p) {
	double result = 0;
    int i = 1;
	while (p != S_EMPTY_LIST) {
        SchemeObject* n = i_car(p);
	    assert_arg_number_type("+", i++, n);
		result += scm2double(n);
        p = i_cdr(p);
	}
	return double2scm(result);
}

SchemeObject* s_minus(SchemeObject* n, SchemeObject* rst) {
    assert_arg_number_type("-", 1, n);
	double result = scm2double(n);

	if (rst == S_EMPTY_LIST) {
	    // One-argument case is a simple negate (n => -n)
    	return double2scm(-result);
	}

    int i = 2;
	while (rst != S_EMPTY_LIST) {
        SchemeObject* cur = i_car(rst);
	    assert_arg_number_type("-", i++, cur);
	    result -= scm2double(cur);
        rst = i_cdr(rst);
	}
	return double2scm(result);
}

SchemeObject* s_divide(SchemeObject* n, SchemeObject* rst) {
    assert_arg_number_type("/", 1, n);
	double result = scm2double(n);

	if (rst == S_EMPTY_LIST) {
        // One-argument case is a simple inverse (n => 1/n)
    	return double2scm(1.0 / result);
	}

    int i = 2;
	while (rst != S_EMPTY_LIST) {
        SchemeObject* cur = i_car(rst);
	    assert_arg_number_type("/", i++, cur);
	    result /= scm2double(cur);
        rst = i_cdr(rst);
	}
	return double2scm(result);
}

SchemeObject* s_mult(SchemeObject* p) {
	double result = 1;
    int i = 1;
	while (p != S_EMPTY_LIST) {
        SchemeObject* cur = i_car(p);
	    assert_arg_number_type("*", i++, cur);
		result *= scm2double(cur);
		p = i_cdr(p);
	}
	return double2scm(result);
}

SchemeObject* s_make_vector(SchemeObject* s_count, SchemeObject* obj) {
    assert_arg_positive_int("make-vector", 1, s_count);
    int count = scm2int(s_count);
    return SchemeObject::createVector(obj, count);
}

SchemeObject* s_vector(SchemeObject* args) {
    int c = scm2int(s_length(args));
    SchemeObject* result = SchemeObject::createVector(S_UNSPECIFIED, c);
    int i = 0;
    while (args != S_EMPTY_LIST) {
        result->setVectorElem(i_car(args), i++);
        args = i_cdr(args);
    }
    return result;
}

SchemeObject* s_vector_length(SchemeObject* v) {
    assert_arg_type("vector-length", 1, s_vector_p, v);
    return int2scm(v->length);
}

SchemeObject* s_list_2_vector(SchemeObject* l) {
    assert_arg_type("list->vector", 1, s_list_p, l);
    return s_vector(l);
}

SchemeObject* s_vector_2_list(SchemeObject* v) {
    assert_arg_type("vector->list", 1, s_vector_p, v);
    SchemeObject* result = S_EMPTY_LIST;
    for(int i = v->length-1; i >= 0; i--) {
	    result = i_cons(v->getVectorElem(i), result);
    }
    return result;
}

SchemeObject* s_vector_ref(SchemeObject* s_v, SchemeObject* s_index) {
    assert_arg_type("vector-ref", 1, s_vector_p, s_v);
    assert_arg_int_in_range("vector-ref", 2, s_index, 0, s_v->length-1);
    int i = scm2int(s_index);
    return s_v->getVectorElem(i);
}

SchemeObject* s_vector_set_e(SchemeObject* s_vec, SchemeObject* s_index, SchemeObject* val) {
    assert_arg_type("vector-set!", 1, s_vector_p, s_vec);
    assert_arg_not_immutable("vector-set!", 1, s_vec);
    assert_arg_int_in_range("vector-set!", 2, s_index, 0, s_vec->length-1);
    uint i = scm2int(s_index);
    s_vec->setVectorElem(val, i);
    return S_UNSPECIFIED;
}

SchemeObject* s_vector_fill_e(SchemeObject* s_vec, SchemeObject* fill) {
    assert_arg_type("vector-fill!", 1, s_vector_p, s_vec);
    assert_arg_not_immutable("vector-fill!", 1, s_vec);
    for(int i = 0; i < s_vec->length; i++) {
	    s_vec->setVectorElem(fill, i);
    }
    return S_UNSPECIFIED;
}

SchemeObject* s_sqrt(SchemeObject* n) {
    assert_arg_number_type("sqrt", 1, n);
    return double2scm(sqrt(scm2double(n)));
}

SchemeObject* s_abs(SchemeObject* n) {
    assert_arg_number_type("abs", 1, n);
    return double2scm(fabs(scm2double(n)));
}


SchemeObject* s_sin(SchemeObject* n) {
    assert_arg_number_type("sin", 1, n);
    return double2scm(sin(scm2double(n)));
}

SchemeObject* s_asin(SchemeObject* n) {
    assert_arg_number_type("asin", 1, n);
    return double2scm(asin(scm2double(n)));
}

SchemeObject* s_cos(SchemeObject* n) {
    assert_arg_number_type("cos", 1, n);
    return double2scm(cos(scm2double(n)));
}

SchemeObject* s_acos(SchemeObject* n) {
    assert_arg_number_type("acos", 1, n);
    return double2scm(acos(scm2double(n)));
}

SchemeObject* s_tan(SchemeObject* n) {
    assert_arg_number_type("tan", 1, n);
    return double2scm(tan(scm2double(n)));
}

SchemeObject* s_atan(SchemeObject* y, SchemeObject* x) {
    assert_arg_number_type("atan", 1, y);
    if (x == S_UNSPECIFIED) {
        return double2scm(atan(scm2double(y)));
    } else {
        assert_arg_number_type("atan", 2, x);
        return double2scm(atan2(scm2double(y), scm2double(x)));
    }
}

SchemeObject* s_log(SchemeObject* n) {
    assert_arg_number_type("log", 1, n);
    return double2scm(log(scm2double(n)));
}

// Returns a^b
SchemeObject* s_expt(SchemeObject* a, SchemeObject* b) {
    assert_arg_number_type("expt", 1, a);
    assert_arg_number_type("expt", 2, b);
    return double2scm(pow(scm2double(a),scm2double(b)));
}

// Returns e^n
SchemeObject* s_exp(SchemeObject* n) {
    assert_arg_number_type("exp", 1, n);
    return double2scm(exp(scm2double(n)));
}

// Round returns the closest integer to x, rounding to even when x is halfway between two integers.
SchemeObject* s_round(SchemeObject* n) {
    assert_arg_number_type("round", 1, n);
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
    return int2scm(int(result));
}

// Ceiling returns the smallest integer not smaller than x
SchemeObject* s_ceiling(SchemeObject* n) {
    assert_arg_number_type("ceiling", 1, n);
    return int2scm(int(ceil(scm2double(n))));
}

// Floor returns the largest integer not larger than x
SchemeObject* s_floor(SchemeObject* n) {
    assert_arg_number_type("floor", 1, n);
    return int2scm(int(floor(scm2double(n))));
}

// Truncate returns the integer closest to x whose absolute value is not larger than the absolute value of x
SchemeObject* s_truncate(SchemeObject* n) {
    assert_arg_number_type("truncate", 1, n);
    return int2scm(int(trunc(scm2double(n))));
}

SchemeObject* s_quotient(SchemeObject* n1, SchemeObject* n2) {
    assert_arg_type("quotient", 1, s_integer_p, n1);
    assert_arg_type("quotient", 2, s_integer_p, n2);
    int nn1 = scm2int(n1);
    int nn2 = scm2int(n2);
    return int2scm(nn1 / nn2);
    
}

SchemeObject* s_remainder(SchemeObject* n1, SchemeObject* n2) {
    assert_arg_type("remainder", 1, s_integer_p, n1);
    assert_arg_type("remainder", 2, s_integer_p, n2);
    int nn1 = scm2int(n1);
    int nn2 = scm2int(n2);
    int result = nn1 % nn2;
    if (result > 0) {
        if (nn1 < 0) {
            result -= abs(nn2);
        }
    } else if (result < 0) {
        if (nn1 > 0) {
            result += abs(nn2);
        }
    }
    return int2scm(result);
}

SchemeObject* s_modulo(SchemeObject* n1, SchemeObject* n2) {
    assert_arg_type("modulo", 1, s_integer_p, n1);
    assert_arg_type("modulo", 2, s_integer_p, n2);
    int nn1 = scm2int(n1);
    int nn2 = scm2int(n2);
    int result = nn1 % nn2;
    if (result * nn2 < 0) {
        if (result > 0) {
            result -= abs(nn2);
        } else {
            result += abs(nn2);
        }
    }
    return int2scm(result);
}


SchemeObject* s_min(SchemeObject* first, SchemeObject* rest) {
    assert_arg_number_type("min", 1, first);
    SchemeObject* result = first;
    double result_number = scm2double(result);
    int i = 2;    
	while (rest != S_EMPTY_LIST) {
        SchemeObject* n = s_car(rest);
	    assert_arg_number_type("min", i++, n);
        double number = scm2double(n);
        if (number < result_number) {
            result_number = number;
            result = n;
        }
        rest = s_cdr(rest);
	}
	return result;
}

SchemeObject* s_max(SchemeObject* first, SchemeObject* rest) {
    assert_arg_number_type("max", 1, first);
    SchemeObject* result = first;
    double result_number = scm2double(result);
    int i = 2;    
	while (rest != S_EMPTY_LIST) {
        SchemeObject* n = s_car(rest);
	    assert_arg_number_type("max", i++, n);
        double number = scm2double(n);
        if (number > result_number) {
            result_number = number;
            result = n;
        }
        rest = s_cdr(rest);
	}
	return result;
}

int gcd(int a, int b) {
    int t = a;
    while(b != 0) {
        t = b;
        b = a % b;
        a = t;
    }
    return t;
}

// Using Euclids algorithm and that gcd is associative thus gcd(a,b,c) = gcd(a,(gcd(b,c))) = gcd(gcd(a,b),c).
SchemeObject* s_gcd(SchemeObject* l) {
    if (i_null_p(l) == S_TRUE) {
        return S_ZERO;
    }
    assert_arg_type("gcd", 1, s_integer_p, s_car(l));
    if (i_null_p(s_cdr(l)) == S_TRUE) {
        return int2scm(abs(scm2int(s_car(l))));
    }
    int a = scm2int(s_car(l));
    int b = scm2int(s_gcd(s_cdr(l)));
    return int2scm(abs(gcd(a,b)));
}

// Using the property gcd(a,b) * lcm(a,b) = a * b and that lcm(a,b,c) = lcm(lcm(a,b),c) = lcm(a,lcm(b,c))
SchemeObject* s_lcm(SchemeObject* l) {
    if (i_null_p(l) == S_TRUE) {
        return S_ONE;
    }
    if (i_null_p(s_cdr(l)) == S_TRUE) {
        assert_arg_type("lcm", 1, s_integer_p, s_car(l));
        return int2scm(abs(scm2int(s_car(l))));
    }

    int a = abs(scm2int(s_car(l)));
    int b = abs(scm2int(s_lcm(s_cdr(l))));
    int g = gcd(a,b);
    int r;
    if (g == 0) {
        r = 0;
    } else {
        r = a * b / g;
    }
    return int2scm(r);
}


SchemeObject* s_even_p(SchemeObject* n) {
    assert_arg_type("even?", 1, s_integer_p, n);
    return (scm2int(n) % 2 == 0) ? S_TRUE : S_FALSE;
}

SchemeObject* s_odd_p(SchemeObject* n) {
    assert_arg_type("odd?", 1, s_integer_p, n);
    return (abs(scm2int(n) % 2) == 1) ? S_TRUE : S_FALSE;
}

SchemeObject* s_zero_p(SchemeObject* n) {
    assert_arg_number_type("zero?", 1, n);
    return scm2double(n) == 0 ? S_TRUE : S_FALSE;
}
SchemeObject* s_negative_p(SchemeObject* n) {
    assert_arg_number_type("negative?", 1, n);
    return scm2double(n) < 0 ? S_TRUE : S_FALSE;
}

SchemeObject* s_positive_p(SchemeObject* n) {
    assert_arg_number_type("positive?", 1, n);
    return scm2double(n) > 0 ? S_TRUE : S_FALSE;
}

SchemeObject* s_equal(SchemeObject* p) {
    if (p == S_EMPTY_LIST) {
        return S_TRUE;
    }
    assert_arg_number_type("=", 1, i_car(p));
    double n = scm2double(s_car(p));
    p = i_cdr(p);
    int i = 2;
    while (p != S_EMPTY_LIST) {
        SchemeObject* car_p = i_car(p);
        assert_arg_number_type("=", i, car_p);
        double nn = scm2double(car_p);
        if (nn != n) {
            return S_FALSE;
        }
        n = nn;
        p = i_cdr(p);
        i++;
    }
    return S_TRUE;
}

SchemeObject* s_less(SchemeObject* p) {
    if (p == S_EMPTY_LIST) {
        return S_TRUE;
    }
    assert_arg_number_type("<", 1, i_car(p));
    double n = scm2double(s_car(p));
    p = s_cdr(p);
    int i = 2;
    while (p != S_EMPTY_LIST) {
        SchemeObject* car_p = i_car(p);
        assert_arg_type("<", i, s_number_p, car_p);
        double nn = scm2double(car_p);
        if (nn <= n) {
            return S_FALSE;
        }
        n = nn;
        p = i_cdr(p);
        i++;
    }
    return S_TRUE;
}

SchemeObject* s_greater(SchemeObject* p) {
    if (p == S_EMPTY_LIST) {
        return S_TRUE;
    }
    assert_arg_number_type(">", 1, i_car(p));
    double n = scm2double(s_car(p));
    p = s_cdr(p);
    int i = 2;
    while (p != S_EMPTY_LIST) {
        SchemeObject* car_p = i_car(p);
        assert_arg_type(">", i, s_number_p, car_p);
        double nn = scm2double(car_p);
        if (nn >= n) {
            return S_FALSE;
        }
        n = nn;
        p = i_cdr(p);
        i++;
    }
    return S_TRUE;
}

SchemeObject* s_less_equal(SchemeObject* p) {
    if (p == S_EMPTY_LIST) {
        return S_TRUE;
    }
    assert_arg_number_type("<=", 1, i_car(p));
    double n = scm2double(s_car(p));
    p = s_cdr(p);
    int i = 2;
    while (p != S_EMPTY_LIST) {
        SchemeObject* car_p = i_car(p);
        assert_arg_type("<=", i, s_number_p, car_p);
        double nn = scm2double(car_p);
        if (nn < n) {
            return S_FALSE;
        }
        n = nn;
        p = i_cdr(p);
        i++;
    }
    return S_TRUE;
}

SchemeObject* s_greater_equal(SchemeObject* p) {
    if (p == S_EMPTY_LIST) {
        return S_TRUE;
    }
    assert_arg_number_type(">=", 1, i_car(p));
    double n = scm2double(s_car(p));
    p = s_cdr(p);
    int i = 2;
    while (p != S_EMPTY_LIST) {
        SchemeObject* car_p = i_car(p);
        assert_arg_type(">=", i, s_number_p, car_p);
        double nn = scm2double(car_p);
        if (nn > n) {
            return S_FALSE;
        }
        n = nn;
        p = i_cdr(p);
        i++;
    }
    return S_TRUE;
}

SchemeObject* s_not(SchemeObject* o) {
    return o == S_FALSE ? S_TRUE : S_FALSE;
}

SchemeObject* s_make_string(SchemeObject* len, SchemeObject* chr) {
    assert_arg_type("make-string", 1, s_integer_p, len);
    
    if (chr == S_UNSPECIFIED) {
        chr = S_SPACE;
    } else {
        assert_arg_type("make-string", 2, s_char_p, chr);
    }

    string s = string(scm2int(len), scm2char(chr));
    return string2scm(s);
}

SchemeObject* s_string(SchemeObject* p) {
    string s = "";
    int i = 0;
    while(p != S_EMPTY_LIST) {
        SchemeObject* c = s_car(p);
        assert_arg_type("string", i, s_char_p, c);
        s += scm2char(c);
        p = s_cdr(p);
    }
    return string2scm(s);
}

SchemeObject* s_string_length(SchemeObject* s) {
    assert_arg_type("string-length", 1, s_string_p, s);
    int len = s->length;
    return int2scm(len);
}

SchemeObject* s_string_ref(SchemeObject* s, SchemeObject* i) {
    assert_arg_type("string-ref", 1, s_string_p, s);
    assert_arg_int_in_range("string-ref", 2, i, 0, s->length-1);
    int index = scm2int(i);
    return char2scm(s->str[index]);
}	

SchemeObject* s_string_set_e(SchemeObject* s, SchemeObject* i, SchemeObject* chr) {
    assert_arg_type("string-set!", 1, s_string_p, s);
    assert_arg_type("string-set!", 3, s_char_p, chr);
    assert_arg_not_immutable("string-set!", 1, s);
    assert_arg_int_in_range("string-set!", 2, i, 0, s->length-1);
    s->str[scm2int(i)] = scm2char(chr);
    return S_UNSPECIFIED;
}

SchemeObject* s_symbol_2_string(SchemeObject* symbol) {
    assert_arg_type("symbol->string", 1, s_symbol_p, symbol);
    SchemeObject* result = SchemeObject::createString(symbol->str);
    // TODO: String constructor shouldn't strdup the string. Just reuse point.
    // That's why we mark it immutable.
    result->set_immutable(true);
    return result;
}

SchemeObject* s_string_2_symbol(SchemeObject* s) {
    assert_arg_type("string->symbol", 1, s_string_p, s);
    return SchemeObject::createSymbol(s->str);
}

SchemeObject* s_string_append(SchemeObject* strings) {
    string result = "";
    int i = 1;
    while (strings != S_EMPTY_LIST) {
        SchemeObject* cur = s_car(strings);
        assert_arg_type("string-append", i++, s_string_p, cur);
        result += scm2string(cur);
        strings = s_cdr(strings);
    }
    return string2scm(result);
}

SchemeObject* s_string_copy(SchemeObject* str) {
    assert_arg_type("string-copy", 1, s_string_p, str);
    return cstr2scm(str->str);
}

SchemeObject* s_substring(SchemeObject* s_str, SchemeObject* s_start, SchemeObject* s_end) {
    assert_arg_type("substring", 1, s_string_p, s_str);
    assert_arg_type("substring", 2, s_integer_p, s_start);
    assert_arg_type("substring", 3, s_integer_p, s_end);
    string str = scm2string(s_str);
    int start = scm2int(s_start);
    int end = scm2int(s_end);
    int len = str.size();
    if (start < 0 || end > len || end < start) {
        throw scheme_exception("substring: index out of range.");
    }
    return string2scm(string(str,start,end-start));
}

SchemeObject* s_number_2_string(SchemeObject* n, SchemeObject* base_s) {
    assert_arg_number_type("number->string", 1, n);
    int base = 10;
    if (base_s != S_UNSPECIFIED) {
        assert_arg_type("number->string", 2, s_integer_p, base_s);
        base = scm2int(base_s);
        if (base != 10 && base != 16 && base != 8) {
            throw scheme_exception("number->string invalid base: " + base_s->toString());
        }
    }
    std::ostringstream ss;
    if (base != 10 ) {
        ss << std::setbase(base) << scm2int(n);
    } else {
        ss << std::setbase(base) << scm2double(n);
    }
    return string2scm(ss.str());
}

SchemeObject* s_string_2_number(SchemeObject* s_string, SchemeObject* base_s) {
    assert_arg_type("string->number", 1, s_string_p, s_string);
    int base = 10;
    if (base_s != S_UNSPECIFIED) {
        assert_arg_type("string->number", 2, s_integer_p, base_s);
        base = scm2int(base_s);
        if (base != 10 && base != 16 && base != 8) {
            throw scheme_exception("string->number invalid base: " + base_s->toString());
        }
    }
    string str = scm2string(s_string);
    istream* is = new istringstream(str);
    double d;
    if (base == 10) {
        (*is) >> std::setbase(base) >> d;
    } else {
        int i;
        (*is) >> std::setbase(base) >> i;
        d = double(i);
    }
    if (!is->eof() || is->fail()) {
        return S_FALSE;
    }
    delete is;
    return double2scm(d);
}


SchemeObject* s_integer_2_char(SchemeObject* i) {
    assert_arg_int_in_range("integer->char", 1, i, 0, 255);
    int n = scm2int(i);
    return char2scm(n);
}

SchemeObject* s_char_2_integer(SchemeObject* c) {
    assert_arg_type("char->integer", 1, s_char_p, c);
    return int2scm(int(scm2char(c)));
}

SchemeObject* s_string_2_list(SchemeObject* s) {
    assert_arg_type("string->list", 1, s_string_p, s);
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
    assert_arg_type("list->string", 1, s_list_p, p);
    string result = "";
    int i = 1;
    while (i_null_p(p) == S_FALSE) {
        assert_arg_type("list->string", i, s_char_p, s_car(p));
        result += scm2char(s_car(p));
        p = s_cdr(p);
        i++;
    }
    return string2scm(result);
}

SchemeObject* s_char_alphabetic_p(SchemeObject* c) {
    assert_arg_type("char-alphabetic?", 1, s_char_p, c);
    return bool2scm(isalpha(scm2char(c)));
}

SchemeObject* s_char_numeric_p(SchemeObject* c) {
    assert_arg_type("char-numeric?", 1, s_char_p, c);
    return bool2scm(isdigit(scm2char(c)));
}

SchemeObject* s_char_whitespace_p(SchemeObject* c) {
    assert_arg_type("char-whitespace?", 1, s_char_p, c);
    return bool2scm(isspace(scm2char(c)));
}

SchemeObject* s_char_upper_case_p(SchemeObject* c) {
    assert_arg_type("char-upper-case?", 1, s_char_p, c);
    return bool2scm(isupper(scm2char(c)));
}

SchemeObject* s_char_lower_case_p(SchemeObject* c) {
    assert_arg_type("char-lower_case?", 1, s_char_p, c);
    return bool2scm(islower(scm2char(c)));
}

SchemeObject* s_char_upcase(SchemeObject* c) {
    assert_arg_type("char-upcase", 1, s_char_p, c);
    return char2scm(toupper(scm2char(c)));    
}

SchemeObject* s_char_downcase(SchemeObject* c) {
    assert_arg_type("char-downcase", 1, s_char_p, c);
    return char2scm(tolower(scm2char(c)));    
}

SchemeObject* s_symgen() {
    ostringstream ss;
    ss << symgen_counter++;
    return SchemeObject::createSymbol(("#G" + ss.str()).c_str());
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
    assert_arg_type("open-input-file", 1, s_string_p, s_filename);
    ifstream* ifs = new ifstream(scm2string(s_filename).c_str(), ios::in);
    if (ifs->fail()) {
        throw scheme_exception("Error opening file " + s_filename->toString());
    }
    return SchemeObject::createInputPort(ifs);
}

SchemeObject* s_open_output_file(SchemeObject* s_filename) {
    assert_arg_type("open-output-file", 1, s_string_p, s_filename);
    ofstream* ofs = new ofstream(scm2string(s_filename).c_str(), ios::out);
    if (ofs->fail()) {
        throw scheme_exception("Error opening file " + s_filename->toString() + " for writing.");
    }
    return SchemeObject::createOutputPort(ofs);
}

SchemeObject* s_close_input_port(SchemeObject* s_port) {
    assert_arg_type("close-input-port", 1, s_input_port_p, s_port);
    istream* is = s_port->is;
    // Only file-streams can be closed in C++
    ifstream* ifs = static_cast<ifstream*>(is);
    if (ifs != NULL) {
       ifs->close();
    }
    return S_UNSPECIFIED;
}

SchemeObject* s_close_output_port(SchemeObject* s_port) {
    assert_arg_type("close-output-port", 1, s_output_port_p, s_port);
    ostream* os = s_port->os;
    // Only file-streams can be closed in C++
    ofstream* ofs = static_cast<ofstream*>(os);
    if (ofs != NULL) {
       ofs->close();
    }
    return S_UNSPECIFIED;
}

SchemeObject* s_call_with_input_file(SchemeObject* s_filename, SchemeObject* s_proc) {
    assert_arg_type("call-with-input-file", 1, s_string_p, s_filename);
    assert_arg_type("call-with-input-file", 2, s_procedure_p, s_proc);
    SchemeObject* input_port = s_open_input_file(s_filename);
    SchemeObject* result = interpreter->call_procedure_1(s_proc, input_port);
    s_close_input_port(input_port);
    return result;
}

SchemeObject* s_call_with_output_file(SchemeObject* s_filename, SchemeObject* s_proc) {
    assert_arg_type("call-with-output-file", 1, s_string_p, s_filename);
    assert_arg_type("call-with-output-file", 2, s_procedure_p, s_proc);
    SchemeObject* output_port = s_open_output_file(s_filename);
    SchemeObject* result = interpreter->call_procedure_1(s_proc, output_port);
    s_close_output_port(output_port);
    return result;
}

SchemeObject* s_with_output_to_file(SchemeObject* s_filename, SchemeObject* s_thunk) {
    assert_arg_type("with-output-to-file", 1, s_string_p, s_filename);
    assert_arg_type("with-output-to-file", 2, s_procedure_p, s_thunk);
    SchemeObject* saved_output_port = current_output_port;
    current_output_port = s_open_output_file(s_filename);
    SchemeObject* result = interpreter->call_procedure_0(s_thunk);
    s_close_output_port(current_output_port);
    current_output_port = saved_output_port;
    return result;
}

SchemeObject* s_with_input_from_file(SchemeObject* s_filename, SchemeObject* s_thunk) {
    assert_arg_type("with-input-from-file", 1, s_string_p, s_filename);
    assert_arg_type("with-input-from-file", 2, s_procedure_p, s_thunk);
    SchemeObject* saved_input_port = current_input_port;
    current_input_port = s_open_input_file(s_filename);
    SchemeObject* result = interpreter->call_procedure_0(s_thunk);
    s_close_input_port(current_input_port);
    current_input_port = saved_input_port;
    return result;
}

SchemeObject* s_read_char(SchemeObject* s_port) {
    istream* is;
    if (s_port == S_UNSPECIFIED) {
        is = s_current_input_port()->is;
    } else {
        assert_arg_type("read-char", 1, s_input_port_p, s_port);
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
    istream* is;
    if (s_port == S_UNSPECIFIED) {
        is = s_current_input_port()->is;
    } else {
        assert_arg_type("peek-char", 1, s_input_port_p, s_port);
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
    assert_arg_type("write-char", 1, s_char_p, s_char);
    ostream* os;
    if (port == S_UNSPECIFIED) {
        os = s_current_output_port()->os;
    } else {
        assert_arg_type("write-char", 2, s_output_port_p, port);
        os = port->os;
    }
    (*os) << scm2char(s_char);
    return S_UNSPECIFIED;
}

SchemeObject* s_read(SchemeObject* s_port) {
    istream* is;
    if (s_port == S_UNSPECIFIED) {
        is = s_current_input_port()->is;
    } else {
        assert_arg_type("read", 1, s_input_port_p, s_port);
        is = s_port->is;
    }
    return global_parser->read(is);
}


SchemeObject* s_write(SchemeObject* o, SchemeObject* port) {
    ostream* os;
    if (port == S_UNSPECIFIED) {
        os = s_current_output_port()->os;
    } else {
        assert_arg_type("write", 2, s_output_port_p, port);
        os = port->os;
    }
    (*os) << o->toString();
    return S_UNSPECIFIED;
}

SchemeObject* s_display(SchemeObject* o, SchemeObject* port) {
    ostream* os;
    if (port == S_UNSPECIFIED) {
        os = s_current_output_port()->os;
    } else {
        assert_arg_type("display", 2, s_output_port_p, port);
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
    ostream* os;
    if (port == S_UNSPECIFIED) {
        os = s_current_output_port()->os;
    } else {
        assert_arg_type("write", 2, s_output_port_p, port);
        os = port->os;
    }
    (*os) << endl;        
    return S_UNSPECIFIED;
}

SchemeObject* s_environment_p(SchemeObject* o) {
    return bool2scm(o->type() == SchemeObject::ENVIRONMENT);
}

SchemeObject* s_null_environment(SchemeObject* s_version) {
    assert_arg_type("null-environment", 1, s_integer_p, s_version);
    if (scm2int(s_version) != 5) {
        throw scheme_exception("Not a valid version. Only 5 is supported.");
    }
    return null_environment;
}

SchemeObject* s_scheme_report_environment(SchemeObject* s_version) {
    assert_arg_type("scheme-report-environment", 1, s_integer_p, s_version);
    if (scm2int(s_version) != 5) {
        throw scheme_exception("scheme-report-environment", "Not a valid version. Only 5 is supported.");
    }
    return scheme_report_environment;
}

SchemeObject* s_interaction_environment(SchemeObject* s_version) {
    assert_arg_type("interaction-environment", 1, s_integer_p, s_version);
    if (scm2int(s_version) != 5) {
        throw scheme_exception("interaction-environment", "Not a valid version. Only 5 is supported.");
    }
    return interaction_environment;
}

SchemeObject* s_eval(SchemeObject* expression, SchemeObject* s_environment) {
    assert_arg_type("eval", 2, s_environment_p, s_environment);
    SchemeObject* expressions = i_cons(expression, S_EMPTY_LIST);
    Interpreter interpreter = Interpreter(expressions, s_environment);
    return interpreter.interpret();
}
