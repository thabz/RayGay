
#include "scheme.h"
#include <sstream>
#include <fstream>
#include <iomanip>
#include <cmath>
#include <cctype>

#include "lexer.h"
#include "parser.h"
#include "interpreter.h"

scheme_exception::scheme_exception(string s) {
    this->str = s;
}

unsigned long symgen_counter = 10000;

SchemeBool* S_TRUE = new SchemeBool(true);
SchemeBool* S_FALSE = new SchemeBool(false);
SchemeEOF* S_EOF = new SchemeEOF();
SchemeNumber* S_ZERO = SchemeNumber::create(0);
SchemeNumber* S_ONE = SchemeNumber::create(1);
SchemeNumber* S_TWO = SchemeNumber::create(2);
SchemeNumber* S_THREE = SchemeNumber::create(3);
SchemeNumber* S_FOUR = SchemeNumber::create(4);
SchemeNumber* S_FIVE = SchemeNumber::create(5);
SchemeNumber* S_SIX = SchemeNumber::create(6);
SchemeNumber* S_SEVEN = SchemeNumber::create(7);
SchemeNumber* S_EIGHT = SchemeNumber::create(8);
SchemeNumber* S_NINE = SchemeNumber::create(9);
SchemeUnspecified* S_UNSPECIFIED = new SchemeUnspecified;
SchemeEmptyList* S_EMPTY_LIST = new SchemeEmptyList();
SchemeChar* S_SPACE = char2scm(' ');
SchemeNumber* S_NUMBERS[] = {S_ZERO, S_ONE, S_TWO, S_THREE, S_FOUR, S_FIVE, S_SIX, S_SEVEN, S_EIGHT, S_NINE};

SchemeSymbol* if_symbol;
SchemeSymbol* cond_symbol;
SchemeSymbol* apply_symbol;
SchemeSymbol* else_symbol;
SchemeSymbol* ergo_symbol;
SchemeSymbol* case_symbol;
SchemeSymbol* do_symbol;
SchemeSymbol* let_symbol;
SchemeSymbol* letstar_symbol;
SchemeSymbol* letrec_symbol;
SchemeSymbol* begin_symbol;
SchemeSymbol* and_symbol;
SchemeSymbol* or_symbol;
SchemeSymbol* lambda_symbol;
SchemeSymbol* quote_symbol;
SchemeSymbol* quasiquote_symbol;
SchemeSymbol* unquote_symbol;
SchemeSymbol* unquote_splicing_symbol;
SchemeSymbol* define_symbol;
SchemeSymbol* define_macro;
SchemeSymbol* set_e_symbol;
SchemeSymbol* unnamed_symbol;

SchemeInputPort* current_input_port = NULL;
SchemeOutputPort* current_output_port = NULL;

Interpreter* interpreter;

SchemeEnvironment* null_environment = SchemeEnvironment::create(NULL);
SchemeEnvironment* scheme_report_environment = SchemeEnvironment::create(null_environment);
SchemeEnvironment* interaction_environment = SchemeEnvironment::create(scheme_report_environment);

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


        assign("if",           new SchemeInternalProcedure("if"),null_environment);
        //assign("apply", new SchemeInternalProcedure("apply"));
        assign("cond",         new SchemeInternalProcedure("cond"),null_environment);
        assign("case",         new SchemeInternalProcedure("case"),null_environment);
        assign("do",           new SchemeInternalProcedure("do"),null_environment);
        assign("let",          new SchemeInternalProcedure("let"),null_environment);
        assign("let*",         new SchemeInternalProcedure("let*"),null_environment);
        assign("letrec",       new SchemeInternalProcedure("letrec"),null_environment);
        assign("begin",        new SchemeInternalProcedure("begin"),null_environment);
        assign("and",          new SchemeInternalProcedure("and"),null_environment);
        assign("or",           new SchemeInternalProcedure("or"),null_environment);
        assign("lambda",       new SchemeInternalProcedure("lambda"),null_environment);
        assign("quote",        new SchemeInternalProcedure("quote"),null_environment);
        assign("quasiquote",   new SchemeInternalProcedure("quasiquote"),null_environment);
        assign("define",       new SchemeInternalProcedure("define"),null_environment);
        assign("define-macro", new SchemeInternalProcedure("define-macro"),null_environment);
        assign("set!",         new SchemeInternalProcedure("set!"),null_environment);

        current_input_port = new SchemeInputPort(&cin);
        current_output_port = new SchemeOutputPort(&cout);

        else_symbol = SchemeSymbol::create("else");
        ergo_symbol = SchemeSymbol::create("=>");
        unquote_symbol = SchemeSymbol::create("unquote");
        unquote_splicing_symbol = SchemeSymbol::create("unquote-splicing");
        unnamed_symbol = SchemeSymbol::create("#<unnamed>");

        if_symbol = SchemeSymbol::create("if");
        apply_symbol = SchemeSymbol::create("apply");
        cond_symbol = SchemeSymbol::create("cond");
        case_symbol = SchemeSymbol::create("case");
        do_symbol = SchemeSymbol::create("do");
        let_symbol = SchemeSymbol::create("let");
        letstar_symbol = SchemeSymbol::create("let*");
        letrec_symbol = SchemeSymbol::create("letrec");
        begin_symbol = SchemeSymbol::create("begin");
        and_symbol = SchemeSymbol::create("and");
        or_symbol = SchemeSymbol::create("or");
        lambda_symbol = SchemeSymbol::create("lambda");
        quote_symbol = SchemeSymbol::create("quote");
        quasiquote_symbol = SchemeSymbol::create("quasiquote");
        define_symbol = SchemeSymbol::create("define");
        define_macro = SchemeSymbol::create("define-macro");
        set_e_symbol = SchemeSymbol::create("set!");
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
    SchemePair* parse_tree = parser->parse(is);
    interpreter = new Interpreter(parse_tree, interaction_environment);
    return interpreter->interpret();
}

SchemeObject* Scheme::eval(string data) {
    istream* is = new istringstream(data);
    SchemeObject* result = eval(is);
    delete is;
    return result;
};

void Scheme::assign(string variable, int req, int opt, int rst, SchemeObject* (*fn)(), SchemeEnvironment* envt = interaction_environment) {
    SchemeSymbol* name = SchemeSymbol::create(variable);
    SchemeProcedure* proc = SchemeProcedure::create(name, req, opt, rst, fn);
    envt->put(name, proc);
}

void Scheme::assign(string variable, SchemeObject* value, SchemeEnvironment* envt = interaction_environment) {
    SchemeSymbol* name = SchemeSymbol::create(variable);
    envt->put(name, value);
}

// -----------------------------------------------------
// Procedures
// -----------------------------------------------------

inline
void assert_arg_type(char* procname, int argnum, SchemeBool* (*test_fn)(SchemeObject*), SchemeObject* arg) {
    if ((*test_fn)(arg) == S_FALSE) {
        ostringstream ss;
        ss << "Wrong argument-type in position ";
        ss << argnum;
        ss << " in call to ";
        ss << string(procname);
        ss << ": " << arg->toString();
        throw scheme_exception(ss.str());
    }
}

inline
void assert_arg_not_immutable(char* procname, int argnum, SchemeObject* arg) {
    if (arg->immutable) {
        ostringstream ss;
        ss << "Can't modify immutable object in position ";
        ss << argnum;
        ss << " in call to " << string(procname);    
        ss << ": " << arg->toString();
        throw scheme_exception(ss.str());
    }
}

inline
void assert_arg_int_in_range(char* procname, int argnum, SchemeObject* arg, int from, int to) {
    assert_arg_type(procname, argnum, s_integer_p, arg);
    int n = scm2int(arg);
    if (n < from || n > to) {
        ostringstream ss;
        ss << "Integer out of range " << from << " to " << to;
        ss << " in position " << argnum;
        ss << " in call to " << procname;   
        ss << ": " << arg->toString();
        throw scheme_exception(ss.str());
    }
}

inline
void assert_arg_positive_int(char* procname, int argnum, SchemeObject* arg) {
    assert_arg_type(procname, argnum, s_integer_p, arg);
    int n = scm2int(arg);
    if (n < 0) {
        ostringstream ss;
        ss << "Negative argument in to position " << argnum;
        ss << " in call to " << string(procname);    
        ss << ": " << arg->toString();
        throw scheme_exception(ss.str());
    }
}


inline
SchemeNumber* make_number(int n) {
    if (n < 10 && n >= 0) {
        return S_NUMBERS[n];
    }
    return SchemeNumber::create(n);
}
// (equal? a b)
// Equal? recursively compares the contents of pairs, vectors, and strings, applying eqv? on other objects 
// such as numbers and symbols. A rule of thumb is that objects are generally equal? if they print the same. 
// Equal? may fail to terminate if its arguments are circular data structures.
SchemeBool* s_equal_p(SchemeObject* a, SchemeObject* b) {
    return a->toString() == b->toString() ? S_TRUE : S_FALSE; 
}

SchemeBool* s_eqv_p(SchemeObject* a, SchemeObject* b) {
    if (a->type() == SchemeObject::NUMBER && b->type() == SchemeObject::NUMBER) {
        double a_n = static_cast<SchemeNumber*>(a)->number;
        double b_n = static_cast<SchemeNumber*>(b)->number;
        return a_n == b_n ? S_TRUE : S_FALSE;
    } else if (a->type() == SchemeObject::CHAR && b->type() == SchemeObject::CHAR) {
        return bool2scm(scm2char(a) == scm2char(b));
    } else {
        return bool2scm(a == b);
    }
}

SchemeBool* s_eq_p(SchemeObject* a, SchemeObject* b) {
    return s_eqv_p(a,b); 
}

// (boolean? b)
SchemeBool* s_char_p(SchemeObject* o) {
    return o->type() == SchemeObject::CHAR ? S_TRUE : S_FALSE;
}


// (boolean? b)
SchemeBool* s_boolean_p(SchemeObject* o) {
    return (o == S_TRUE || o == S_FALSE) ? S_TRUE : S_FALSE;
}

// (list? a)
SchemeBool* s_list_p(SchemeObject* o) {
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

SchemeBool* s_circular_list_p(SchemeObject* o) {
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



// TODO: G�r denne til en intern function med tail-optimization
SchemeObject* s_call_cc(SchemeObject* s_proc) {
    assert_arg_type("proc", 1, s_procedure_p, s_proc);
    SchemeContinuation* s_escape = new SchemeContinuation();
    size_t stack_size = stack.size();
    stack.push_back(s_escape);
    int kk = setjmp(s_escape->jmpbuf);
    if (kk == 0) {
        SchemeObject* result = interpreter->call_procedure_1(s_proc, s_escape);
        // s_proc didn't call the escape-continuation
        stack.resize(stack_size);
        return result;
    }
    stack.resize(stack_size);
    return s_escape->result;
}

// TODO: G�r denne til en intern function (igen) med tail-optimization i kaldet til proc.
// TODO: Det er m�ske nemmere at skrive denne som en macro, ala (define-macro (apply proc args) `(,proc ,@args))
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
                    collected = static_cast<SchemePair*>(arg);
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
                SchemePair* tmp = s_cons(arg,S_EMPTY_LIST);
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
    
    SchemePair* result = S_EMPTY_LIST;
    SchemeObject* prev = S_EMPTY_LIST;

    // Vi skralder af lists i hvert genneml�b. S� ((1 2 3)(10 20 30)) bliver til ((2 3)(20 30)) og til sidst ((3)(30))
    while (s_car(lists) != S_EMPTY_LIST) {
        // Collect args
        SchemePair* collection = S_EMPTY_LIST;
        SchemePair* prev_col = S_EMPTY_LIST;
        SchemeObject* lists_ptr = lists;
        while (lists_ptr != S_EMPTY_LIST) {
            SchemeObject* arg = s_car(s_car(lists_ptr));
            s_set_car_e(lists_ptr,s_cdr(s_car(lists_ptr)));
            if (collection == S_EMPTY_LIST) {
                collection = s_cons(arg,S_EMPTY_LIST);
                prev_col = collection;
            } else {
                SchemePair* tmp = s_cons(arg,S_EMPTY_LIST);
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
            SchemePair* tmp = s_cons(result_item, S_EMPTY_LIST);
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
    stack.pop_back();
    return result;    
}

SchemeObject* s_for_each(SchemeObject* proc, SchemeObject* lists) {
    s_map(proc, lists);
    return S_UNSPECIFIED;
}


SchemeObject* member_helper(SchemeBool* (comparator)(SchemeObject*,SchemeObject*), SchemeObject* obj, SchemeObject* p) {
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

SchemeObject* assoc_helper(SchemeBool* (comparator)(SchemeObject*,SchemeObject*), SchemeObject* obj, SchemeObject* alist) {
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

SchemeObject* s_assoc(SchemeObject* obj, SchemePair* alist) {
    return assoc_helper(s_equal_p, obj, alist); 
}

SchemeObject* s_assq(SchemeObject* obj, SchemePair* alist) {
    return assoc_helper(s_eq_p, obj, alist); 
}

SchemeObject* s_assv(SchemeObject* obj, SchemePair* alist) {
    return assoc_helper(s_eqv_p, obj, alist); 
}

// (pair? p)
SchemeBool* s_pair_p(SchemeObject* o) {
    return i_pair_p(o);
}

// (symbol? p)
SchemeBool* s_symbol_p(SchemeObject* o) {
    return i_symbol_p(o);
}

// (string? p)
SchemeBool* s_string_p(SchemeObject* p) {
    return (p->type() == SchemeObject::STRING) ? S_TRUE : S_FALSE;
}

// (procedure? p)
SchemeBool* s_procedure_p(SchemeObject* p) {
    return (p->type() == SchemeObject::PROCEDURE || 
            p->type() == SchemeObject::CONTINUATION ||
            p->type() == SchemeObject::INTERNAL_PROCEDURE) ? S_TRUE : S_FALSE;
}

// (number? p)
SchemeBool* s_number_p(SchemeObject* p) {
    return (p->type() == SchemeObject::NUMBER) ? S_TRUE : S_FALSE;
}

// (integer? p)
SchemeBool* s_integer_p(SchemeObject* p) {
    if (p->type() != SchemeObject::NUMBER) {
        return S_FALSE;
    }
    return (static_cast<SchemeNumber*>(p)->number == s_round(p)->number) ? S_TRUE : S_FALSE;
}

SchemeBool* s_complex_p(SchemeObject* n) {
    return S_TRUE;
}

SchemeBool* s_rational_p(SchemeObject* n) {
    return S_TRUE;
}

SchemeBool* s_real_p(SchemeObject* n) {
    return S_TRUE;
}

SchemeBool* s_exact_p(SchemeObject* n) {
    assert_arg_type("exact?", 1, s_number_p, n);
    return s_integer_p(n);
}

SchemeBool* s_inexact_p(SchemeObject* n) {
    assert_arg_type("inexact?", 1, s_number_p, n);
    return s_exact_p(n) == S_TRUE ? S_FALSE : S_TRUE;
}

// (number? p)
SchemeBool* s_vector_p(SchemeObject* p) {
    return (p->type() == SchemeObject::VECTOR) ? S_TRUE : S_FALSE;
}

// (null? p)
SchemeBool* s_null_p(SchemeObject* p) {
    return i_null_p(p);
}

SchemeObject* s_car(SchemeObject* o) {
    assert_arg_type("car", 1, s_pair_p, o);
    return i_car(o);
}

SchemeObject* s_cdr(SchemeObject* o) {
    assert_arg_type("cdr", 1, s_pair_p, o);
    return i_cdr(o);
}

SchemeObject* s_cxr(SchemeObject* o, char* x) {
    while (x[0] != '\0') {
        if (x[0] == 'a') {
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

// (cons a b)
SchemePair* s_cons(SchemeObject* car, SchemeObject* cdr) {
    return SchemePair::create(car, cdr);
}

SchemeObject* s_list(SchemeObject* args) {
    return args;
}

SchemePair* s_reverse(SchemeObject* o) {
    if (o != S_EMPTY_LIST) {
        assert_arg_type("reverse", 1, s_pair_p, o);
    }
    SchemePair* result = S_EMPTY_LIST;
    while (o != S_EMPTY_LIST) {
		result = s_cons(s_car(o), result);
		o = s_cdr(o);
        if (o != S_EMPTY_LIST) {
            assert_arg_type("reverse", 1, s_pair_p, o);
        }
	}
	return result;  
}

SchemeNumber* s_length(SchemeObject* p) {
    if (p != S_EMPTY_LIST) {
        assert_arg_type("length", 1, s_pair_p, p);
    }
    int length = 0;
    while (p != S_EMPTY_LIST) {
        length++;
        p = s_cdr(p);
        if (p != S_EMPTY_LIST) {
            assert_arg_type("length", 1, s_pair_p, p);
        }
    }
    return make_number(length);
}

SchemeObject* s_set_car_e(SchemeObject* p, SchemeObject* o) {
    assert_arg_type("set-car!", 1, s_pair_p, p);
    assert_arg_not_immutable("set-car!", 1, p);
    static_cast<SchemePair*>(p)->car = o;
    return S_UNSPECIFIED;
}

SchemeObject* s_set_cdr_e(SchemeObject* p, SchemeObject* o) {
    assert_arg_type("set-cdr!", 1, s_pair_p, p);
    assert_arg_not_immutable("set-cdr!", 1, p);
    static_cast<SchemePair*>(p)->cdr = o;
    return S_UNSPECIFIED;
}

SchemeObject* s_append(SchemeObject* p) {
    SchemePair* result = S_EMPTY_LIST;
    SchemePair* prev = NULL;
    SchemePair* tmp;
    
    if (p == S_EMPTY_LIST) {
        return S_EMPTY_LIST;
    }
    
    if (s_cdr(p) == S_EMPTY_LIST) {
        return s_car(p);
    }
    int i = 1;
    while (s_cdr(p) != S_EMPTY_LIST) {
        SchemeObject* pp = s_car(p);
        if (pp == S_EMPTY_LIST) {
            // Skip empty lists
            p = s_cdr(p);
            continue;
        } 
        
        assert_arg_type("append", i, s_pair_p, pp);

	    while (pp != S_EMPTY_LIST) {
	        if (result == S_EMPTY_LIST) {
                result = s_cons(s_car(pp), S_EMPTY_LIST);
                prev = result;
	        } else {
	            tmp = s_cons(s_car(pp), S_EMPTY_LIST);
                prev->cdr = tmp;
                prev = tmp;
            }
            pp = s_cdr(pp);

            if (pp != S_EMPTY_LIST) {
                assert_arg_type("append", i, s_pair_p, pp);
            }
	    }
        i++;
        p = s_cdr(p);
    }

    // Appends final arg
    if (prev != NULL) {
        prev->cdr = s_car(p);
    } else {
        return s_car(p);
    }
    return result;
}

SchemeNumber* s_plus(SchemeObject* p) {
	double result = 0;
    int i = 1;
	while (p != S_EMPTY_LIST) {
	    assert_arg_type("+", i++, s_number_p, s_car(p));
		result += scm2double(s_car(p));
        p = s_cdr(p);
	}
	return SchemeNumber::create(result);
}

SchemeNumber* s_minus(SchemeObject* n, SchemeObject* rst) {
    assert_arg_type("-", 1, s_number_p, n);
	double result = scm2double(n);

	if (rst == S_EMPTY_LIST) {
	    // One-argument case is a simple negate (n => -n)
    	return SchemeNumber::create(-result);
	}

    int i = 2;
	while (rst != S_EMPTY_LIST) {
        SchemeObject* cur = s_car(rst);
	    assert_arg_type("-", i++, s_number_p, cur);
	    result -= scm2double(cur);
        rst = s_cdr(rst);
	}
	return SchemeNumber::create(result);
}

SchemeNumber* s_divide(SchemeObject* n, SchemeObject* rst) {
    assert_arg_type("/", 1, s_number_p, n);
	double result = scm2double(n);

	if (rst == S_EMPTY_LIST) {
        // One-argument case is a simple inverse (n => 1/n)
    	return SchemeNumber::create(1.0 / result);
	}

    int i = 2;
	while (rst != S_EMPTY_LIST) {
        SchemeObject* cur = s_car(rst);
	    assert_arg_type("/", i++, s_number_p, cur);
	    result /= scm2double(cur);
        rst = s_cdr(rst);
	}
	return SchemeNumber::create(result);
}

SchemeNumber* s_mult(SchemeObject* p) {
	double result = 1;
    int i = 1;
	while (p != S_EMPTY_LIST) {
	    assert_arg_type("*", i++, s_number_p, s_car(p));
		double number = scm2double(s_car(p));
		result *= number;
		p = s_cdr(p);
	}
	return SchemeNumber::create(result);
}

SchemeVector* s_make_vector(SchemeObject* s_count, SchemeObject* obj) {
    assert_arg_positive_int("make-vector", 1, s_count);
    int count = scm2int(s_count);
    return SchemeVector::create(obj, count);
}

SchemeVector* s_vector(SchemeObject* args) {
    int c = scm2int(s_length(args));
    SchemeObject** elems = new SchemeObject*[c];
    int i = 0;
    while (args != S_EMPTY_LIST) {
        elems[i++] = s_car(args);
        args = s_cdr(args);
    }
    return SchemeVector::create(elems,c);
}

SchemeNumber* s_vector_length(SchemeObject* v) {
    assert_arg_type("vector-length", 1, s_vector_p, v);
    SchemeVector* vv = static_cast<SchemeVector*>(v);
    return make_number(vv->length);
}

SchemeVector* s_list_2_vector(SchemeObject* l) {
    assert_arg_type("list->vector", 1, s_list_p, l);
    return s_vector(l);
}

SchemePair* s_vector_2_list(SchemeObject* o) {
    assert_arg_type("vector->list", 1, s_vector_p, o);
    SchemeVector* v = static_cast<SchemeVector*>(o);
    SchemePair* result = S_EMPTY_LIST;
    for(int i = v->length-1; i >= 0; i--) {
	    result = s_cons(v->get(i), result);
    }
    return result;
}

SchemeObject* s_vector_ref(SchemeObject* s_v, SchemeObject* s_index) {
    assert_arg_type("vector-ref", 1, s_vector_p, s_v);
    SchemeVector* v = static_cast<SchemeVector*>(s_v);
    assert_arg_int_in_range("vector-ref", 2, s_index, 0, v->length-1);
    int i = scm2int(s_index);
    return v->get(i);
}

SchemeObject* s_vector_set_e(SchemeObject* s_vec, SchemeObject* s_index, SchemeObject* val) {
    assert_arg_type("vector-set!", 1, s_vector_p, s_vec);
    assert_arg_not_immutable("vector-set!", 1, s_vec);
    SchemeVector* vec = static_cast<SchemeVector*>(s_vec);
    assert_arg_int_in_range("vector-set!", 2, s_index, 0, vec->length-1);
    int i = scm2int(s_index);
    vec->set(val, i);
    return S_UNSPECIFIED;
}

SchemeObject* s_vector_fill_e(SchemeObject* s_vec, SchemeObject* fill) {
    assert_arg_type("vector-fill!", 1, s_vector_p, s_vec);
    assert_arg_not_immutable("vector-fill!", 1, s_vec);
    SchemeVector* vec = static_cast<SchemeVector*>(s_vec);
    for(int i = 0; i < vec->length; i++) {
	    vec->set(fill, i);
    }
    return S_UNSPECIFIED;
}

SchemeNumber* s_sqrt(SchemeObject* n) {
    assert_arg_type("sqrt", 1, s_number_p, n);
    return SchemeNumber::create(sqrt(scm2double(n)));
}

SchemeNumber* s_abs(SchemeObject* n) {
    assert_arg_type("abs", 1, s_number_p, n);
    return SchemeNumber::create(fabs(scm2double(n)));
}


SchemeNumber* s_sin(SchemeObject* n) {
    assert_arg_type("sin", 1, s_number_p, n);
    return SchemeNumber::create(sin(scm2double(n)));
}

SchemeNumber* s_asin(SchemeObject* n) {
    assert_arg_type("asin", 1, s_number_p, n);
    return SchemeNumber::create(asin(scm2double(n)));
}

SchemeNumber* s_cos(SchemeObject* n) {
    assert_arg_type("cos", 1, s_number_p, n);
    return SchemeNumber::create(cos(scm2double(n)));
}

SchemeNumber* s_acos(SchemeObject* n) {
    assert_arg_type("acos", 1, s_number_p, n);
    return SchemeNumber::create(acos(scm2double(n)));
}

SchemeNumber* s_tan(SchemeObject* n) {
    assert_arg_type("tan", 1, s_number_p, n);
    return SchemeNumber::create(tan(scm2double(n)));
}

SchemeNumber* s_atan(SchemeObject* y, SchemeObject* x) {
    assert_arg_type("atan", 1, s_number_p, y);
    if (x == S_UNSPECIFIED) {
        return SchemeNumber::create(atan(scm2double(y)));
    } else {
        assert_arg_type("atan", 2, s_number_p, x);
        return SchemeNumber::create(atan2(scm2double(y), scm2double(x)));
    }
}

SchemeNumber* s_log(SchemeObject* n) {
    assert_arg_type("log", 1, s_number_p, n);
    return SchemeNumber::create(log(scm2double(n)));
}

// Returns a^b
SchemeNumber* s_expt(SchemeObject* a, SchemeObject* b) {
    assert_arg_type("expt", 1, s_number_p, a);
    assert_arg_type("expt", 1, s_number_p, b);
    return SchemeNumber::create(pow(scm2double(a),scm2double(b)));
}

// Returns e^n
SchemeNumber* s_exp(SchemeObject* n) {
    assert_arg_type("exp", 1, s_number_p, n);
    return SchemeNumber::create(exp(scm2double(n)));
}

// Round returns the closest integer to x, rounding to even when x is halfway between two integers.
SchemeNumber* s_round(SchemeObject* n) {
    assert_arg_type("round", 1, s_number_p, n);
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
    return SchemeNumber::create(result);
}

// Ceiling returns the smallest integer not smaller than x
SchemeNumber* s_ceiling(SchemeObject* n) {
    assert_arg_type("ceiling", 1, s_number_p, n);
    return SchemeNumber::create(ceil(scm2double(n)));
}

// Floor returns the largest integer not larger than x
SchemeNumber* s_floor(SchemeObject* n) {
    assert_arg_type("floor", 1, s_number_p, n);
    return SchemeNumber::create(floor(scm2double(n)));
}

// Truncate returns the integer closest to x whose absolute value is not larger than the absolute value of x
SchemeNumber* s_truncate(SchemeObject* n) {
    assert_arg_type("truncate", 1, s_number_p, n);
    return SchemeNumber::create(trunc(scm2double(n)));
}

SchemeNumber* s_quotient(SchemeObject* n1, SchemeObject* n2) {
    assert_arg_type("quotient", 1, s_integer_p, n1);
    assert_arg_type("quotient", 2, s_integer_p, n2);
    int nn1 = int(scm2int(n1));
    int nn2 = int(scm2int(n2));
    return make_number(nn1 / nn2);
    
}

SchemeNumber* s_remainder(SchemeObject* n1, SchemeObject* n2) {
    assert_arg_type("remainder", 1, s_integer_p, n1);
    assert_arg_type("remainder", 2, s_integer_p, n2);
    int nn1 = int(scm2int(n1));
    int nn2 = int(scm2int(n2));
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
    return make_number(result);
}

SchemeNumber* s_modulo(SchemeObject* n1, SchemeObject* n2) {
    assert_arg_type("modulo", 1, s_integer_p, n1);
    assert_arg_type("modulo", 2, s_integer_p, n2);
    int nn1 = int(scm2int(n1));
    int nn2 = int(scm2int(n2));
    int result = nn1 % nn2;
    if (result * nn2 < 0) {
        if (result > 0) {
            result -= abs(nn2);
        } else {
            result += abs(nn2);
        }
    }
    return make_number(result);
}


SchemeObject* s_min(SchemeObject* first, SchemeObject* rest) {
    assert_arg_type("min", 1, s_number_p, first);
    SchemeObject* result = first;
    double result_number = scm2double(result);
    int i = 2;    
	while (rest != S_EMPTY_LIST) {
        SchemeObject* n = s_car(rest);
	    assert_arg_type("min", i++, s_number_p, n);
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
    assert_arg_type("max", 1, s_number_p, first);
    SchemeObject* result = first;
    double result_number = scm2double(result);
    int i = 2;    
	while (rest != S_EMPTY_LIST) {
        SchemeObject* n = s_car(rest);
	    assert_arg_type("max", i++, s_number_p, n);
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
    assert_arg_type("gcd", 1, s_pair_p, l);
    assert_arg_type("gcd", 1, s_integer_p, s_car(l));
    if (i_null_p(s_cdr(l)) == S_TRUE) {
        return make_number(abs(scm2int(s_car(l))));
    }
    int a = scm2int(s_car(l));
    int b = scm2int(s_gcd(s_cdr(l)));
    return make_number(abs(gcd(a,b)));
}

// Using the property gcd(a,b) * lcm(a,b) = a * b and that lcm(a,b,c) = lcm(lcm(a,b),c) = lcm(a,lcm(b,c))
SchemeNumber* s_lcm(SchemeObject* l) {
    if (i_null_p(l) == S_TRUE) {
        return S_ONE;
    }
    assert_arg_type("lcm", 1, s_pair_p, l);
    if (i_null_p(s_cdr(l)) == S_TRUE) {
        assert_arg_type("lcm", 1, s_integer_p, s_car(l));
        return make_number(abs(scm2int(s_car(l))));
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
    return make_number(r);
}


SchemeBool* s_even_p(SchemeObject* n) {
    assert_arg_type("even?", 1, s_integer_p, n);
    return (scm2int(n) % 2 == 0) ? S_TRUE : S_FALSE;
}

SchemeBool* s_odd_p(SchemeObject* n) {
    assert_arg_type("odd?", 1, s_integer_p, n);
    return (abs(scm2int(n) % 2) == 1) ? S_TRUE : S_FALSE;
}

SchemeBool* s_zero_p(SchemeObject* n) {
    assert_arg_type("zero?", 1, s_number_p, n);
    return scm2double(n) == 0 ? S_TRUE : S_FALSE;
}
SchemeBool* s_negative_p(SchemeObject* n) {
    assert_arg_type("negative?", 1, s_number_p, n);
    return scm2double(n) < 0 ? S_TRUE : S_FALSE;
}

SchemeBool* s_positive_p(SchemeObject* n) {
    assert_arg_type("position?", 1, s_number_p, n);
    return scm2double(n) > 0 ? S_TRUE : S_FALSE;
}

SchemeBool* s_equal(SchemeObject* p) {
    if (p == S_EMPTY_LIST) {
        return S_TRUE;
    }
    assert_arg_type("=", 1, s_number_p, s_car(p));
    double n = scm2double(s_car(p));
    p = s_cdr(p);
    int i = 2;
    while (p != S_EMPTY_LIST) {
        SchemeObject* car_p = s_car(p);
        assert_arg_type("=", i, s_number_p, car_p);
        double nn = scm2double(car_p);
        if (nn != n) {
            return S_FALSE;
        }
        n = nn;
        p = s_cdr(p);
        i++;
    }
    return S_TRUE;
}

SchemeBool* s_less(SchemeObject* p) {
    if (p == S_EMPTY_LIST) {
        return S_TRUE;
    }
    assert_arg_type("<", 1, s_number_p, s_car(p));
    double n = scm2double(s_car(p));
    p = s_cdr(p);
    int i = 2;
    while (p != S_EMPTY_LIST) {
        SchemeObject* car_p = s_car(p);
        assert_arg_type("<", i, s_number_p, car_p);
        double nn = scm2double(car_p);
        if (nn <= n) {
            return S_FALSE;
        }
        n = nn;
        p = s_cdr(p);
        i++;
    }
    return S_TRUE;
}

SchemeBool* s_greater(SchemeObject* p) {
    if (p == S_EMPTY_LIST) {
        return S_TRUE;
    }
    assert_arg_type(">", 1, s_number_p, s_car(p));
    double n = scm2double(s_car(p));
    p = s_cdr(p);
    int i = 2;
    while (p != S_EMPTY_LIST) {
        SchemeObject* car_p = s_car(p);
        assert_arg_type(">", i, s_number_p, car_p);
        double nn = scm2double(car_p);
        if (nn >= n) {
            return S_FALSE;
        }
        n = nn;
        p = s_cdr(p);
        i++;
    }
    return S_TRUE;
}

SchemeBool* s_less_equal(SchemeObject* p) {
    if (p == S_EMPTY_LIST) {
        return S_TRUE;
    }
    assert_arg_type("<=", 1, s_number_p, s_car(p));
    double n = scm2double(s_car(p));
    p = s_cdr(p);
    int i = 2;
    while (p != S_EMPTY_LIST) {
        SchemeObject* car_p = s_car(p);
        assert_arg_type("<=", i, s_number_p, car_p);
        double nn = scm2double(car_p);
        if (nn < n) {
            return S_FALSE;
        }
        n = nn;
        p = s_cdr(p);
        i++;
    }
    return S_TRUE;
}

SchemeBool* s_greater_equal(SchemeObject* p) {
    if (p == S_EMPTY_LIST) {
        return S_TRUE;
    }
    assert_arg_type(">=", 1, s_number_p, s_car(p));
    double n = scm2double(s_car(p));
    p = s_cdr(p);
    int i = 2;
    while (p != S_EMPTY_LIST) {
        SchemeObject* car_p = s_car(p);
        assert_arg_type(">=", i, s_number_p, car_p);
        double nn = scm2double(car_p);
        if (nn > n) {
            return S_FALSE;
        }
        n = nn;
        p = s_cdr(p);
        i++;
    }
    return S_TRUE;
}

SchemeBool* s_not(SchemeObject* o) {
    return o == S_FALSE ? S_TRUE : S_FALSE;
}

SchemeString* s_make_string(SchemeObject* len, SchemeObject* chr) {
    assert_arg_type("make-string", 1, s_number_p, len);
    
    if (chr == S_UNSPECIFIED) {
        chr = S_SPACE;
    } else {
        assert_arg_type("make-string", 2, s_char_p, chr);
    }

    string s = string(int(static_cast<SchemeNumber*>(len)->number), static_cast<SchemeChar*>(chr)->c);
    return SchemeString::create(s);
}

SchemeString* s_string(SchemeObject* p) {
    string s = "";
    int i = 0;
    while(p != S_EMPTY_LIST) {
        SchemeObject* c = s_car(p);
        assert_arg_type("string", i, s_char_p, c);
        s += scm2char(c);
        p = s_cdr(p);
    }
    return SchemeString::create(s);
}

SchemeNumber* s_string_length(SchemeObject* s) {
    assert_arg_type("string-length", 1, s_string_p, s);
    int len = static_cast<SchemeString*>(s)->str.size();
    return make_number(len);
}

SchemeChar* s_string_ref(SchemeObject* s, SchemeNumber* i) {
    assert_arg_type("string-ref", 1, s_string_p, s);
    assert_arg_type("string-ref", 2, s_number_p, i);
    int index = int(i->number);
    string& ss = scm2string(s);
    assert_arg_int_in_range("string-ref", 2, i, 0, ss.size()-1);
    return char2scm(ss[index]);
}	

SchemeObject* s_string_set_e(SchemeObject* s, SchemeObject* i, SchemeObject* chr) {
    assert_arg_type("string-set!", 1, s_string_p, s);
    assert_arg_type("string-set!", 2, s_number_p, i);
    assert_arg_type("string-set!", 3, s_char_p, chr);
    assert_arg_not_immutable("string-set!", 1, s);
    string& ss = scm2string(s);
    assert_arg_int_in_range("string-set!", 2, i, 0, ss.size()-1);
    ss[scm2int(i)] = scm2char(chr);
    return S_UNSPECIFIED;
}

SchemeString* s_symbol_2_string(SchemeObject* symbol) {
    assert_arg_type("symbol->string", 1, s_symbol_p, symbol);
    return SchemeString::create(static_cast<SchemeSymbol*>(symbol)->str,true);
}

SchemeSymbol* s_string_2_symbol(SchemeObject* s) {
    assert_arg_type("string->symbol", 1, s_string_p, s);
    return SchemeSymbol::create(scm2string(s));
}

SchemeString* s_string_append(SchemeObject* strings) {
    string result = "";
    int i = 1;
    while (strings != S_EMPTY_LIST) {
        SchemeObject* car_strings = s_car(strings);
        assert_arg_type("string-append", i++, s_string_p, car_strings);
        result += scm2string(car_strings);
        strings = s_cdr(strings);
    }
    return SchemeString::create(result);
}

SchemeString* s_string_copy(SchemeObject* str) {
    assert_arg_type("string-copy", 1, s_string_p, str);
    return SchemeString::create(static_cast<SchemeString*>(str)->str,false);
}

SchemeString* s_substring(SchemeObject* s_str, SchemeObject* s_start, SchemeObject* s_end) {
    assert_arg_type("substring", 1, s_string_p, s_str);
    assert_arg_type("substring", 2, s_integer_p, s_start);
    assert_arg_type("substring", 3, s_integer_p, s_end);
    string str = scm2string(s_str);
    int start = scm2int(s_start);
    int end = scm2int(s_end);
    int len = str.size();
    if (start < 0 || end > len) {
        throw scheme_exception("substring: index out of range.");
    }
    return string2scm(string(str,start,end-start));
}

SchemeObject* s_number_2_string(SchemeObject* n, SchemeObject* base_s) {
    assert_arg_type("number->string", 1, s_number_p, n);
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
    return SchemeString::create(ss.str());
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
    return SchemeNumber::create(d);
}


SchemeChar* s_integer_2_char(SchemeObject* i) {
    assert_arg_int_in_range("integer->char", 1, i, 0, 255);
    int n = scm2int(i);
    return char2scm(n);
}

SchemeNumber* s_char_2_integer(SchemeObject* c) {
    assert_arg_type("char->integer", 1, s_char_p, c);
    return make_number(int(static_cast<SchemeChar*>(c)->c));
}

SchemePair* s_string_2_list(SchemeObject* s) {
    assert_arg_type("string->list", 1, s_string_p, s);
    string& ss = scm2string(s);
    SchemePair* result = S_EMPTY_LIST;
    SchemePair* result_tail = S_EMPTY_LIST;
    for(uint i = 0; i < ss.size(); i++) {
	SchemePair* newpair = s_cons(char2scm(ss[i]), S_EMPTY_LIST);
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

SchemeString* s_list_2_string(SchemeObject* p) {
    assert_arg_type("list->string", 1, s_list_p, p);
    string result = "";
    int i = 1;
    while (i_null_p(p) == S_FALSE) {
        assert_arg_type("list->string", i, s_char_p, s_car(p));
        result += scm2char(s_car(p));
        p = s_cdr(p);
        i++;
    }
    return SchemeString::create(result);
}

SchemeBool* s_char_alphabetic_p(SchemeObject* c) {
    assert_arg_type("char-alphabetic?", 1, s_char_p, c);
    return bool2scm(isalpha(scm2char(c)));
}

SchemeBool* s_char_numeric_p(SchemeObject* c) {
    assert_arg_type("char-numeric?", 1, s_char_p, c);
    return bool2scm(isdigit(scm2char(c)));
}

SchemeBool* s_char_whitespace_p(SchemeObject* c) {
    assert_arg_type("char-whitespace?", 1, s_char_p, c);
    return bool2scm(isspace(scm2char(c)));
}

SchemeBool* s_char_upper_case_p(SchemeObject* c) {
    assert_arg_type("char-upper-case?", 1, s_char_p, c);
    return bool2scm(isupper(scm2char(c)));
}

SchemeBool* s_char_lower_case_p(SchemeObject* c) {
    assert_arg_type("char-lower_case?", 1, s_char_p, c);
    return bool2scm(islower(scm2char(c)));
}

SchemeChar* s_char_upcase(SchemeObject* c) {
    assert_arg_type("char-upcase", 1, s_char_p, c);
    return char2scm(toupper(scm2char(c)));    
}

SchemeChar* s_char_downcase(SchemeObject* c) {
    assert_arg_type("char-downcase", 1, s_char_p, c);
    return char2scm(tolower(scm2char(c)));    
}

SchemeSymbol* s_symgen() {
    ostringstream ss;
    ss << symgen_counter++;
    return SchemeSymbol::create("#G" + ss.str());
}

SchemeOutputPort* s_current_output_port() {
    return current_output_port;
}

SchemeInputPort* s_current_input_port() {
    return current_input_port;
}

SchemeBool* s_input_port_p(SchemeObject* o) {
    return o->type() == SchemeObject::INPUT_PORT ? S_TRUE : S_FALSE;
}

SchemeBool* s_output_port_p(SchemeObject* o) {
    return o->type() == SchemeObject::OUTPUT_PORT ? S_TRUE : S_FALSE;
}

SchemeBool* s_eof_object_p(SchemeObject* o) {
    return bool2scm(o->type() == SchemeObject::EOFTYPE);
}

SchemeInputPort* s_open_input_file(SchemeObject* s_filename) {
    assert_arg_type("open-input-file", 1, s_string_p, s_filename);
    ifstream* ifs = new ifstream(scm2string(s_filename).c_str(), ios::in);
    if (ifs->fail()) {
        throw scheme_exception("Error opening file " + s_filename->toString());
    }
    return new SchemeInputPort(ifs);
}

SchemeOutputPort* s_open_output_file(SchemeObject* s_filename) {
    assert_arg_type("open-output-file", 1, s_string_p, s_filename);
    ofstream* ofs = new ofstream(scm2string(s_filename).c_str(), ios::out);
    if (ofs->fail()) {
        throw scheme_exception("Error opening file " + s_filename->toString() + " for writing.");
    }
    return new SchemeOutputPort(ofs);
}

SchemeObject* s_close_input_port(SchemeObject* s_port) {
    assert_arg_type("close-input-port", 1, s_input_port_p, s_port);
    istream* is = static_cast<SchemeInputPort*>(s_port)->is;
    // Only file-streams can be closed in C++
    ifstream* ifs = static_cast<ifstream*>(is);
    if (ifs != NULL) {
       ifs->close();
    }
    return S_UNSPECIFIED;
}

SchemeObject* s_close_output_port(SchemeObject* s_port) {
    assert_arg_type("close-output-port", 1, s_output_port_p, s_port);
    ostream* os = static_cast<SchemeOutputPort*>(s_port)->os;
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
    SchemeInputPort* input_port = s_open_input_file(s_filename);
    SchemeObject* result = interpreter->call_procedure_1(s_proc, input_port);
    s_close_input_port(input_port);
    return result;
}

SchemeObject* s_call_with_output_file(SchemeObject* s_filename, SchemeObject* s_proc) {
    assert_arg_type("call-with-output-file", 1, s_string_p, s_filename);
    assert_arg_type("call-with-output-file", 2, s_procedure_p, s_proc);
    SchemeOutputPort* output_port = s_open_output_file(s_filename);
    SchemeObject* result = interpreter->call_procedure_1(s_proc, output_port);
    s_close_output_port(output_port);
    return result;
}

SchemeObject* s_with_output_to_file(SchemeObject* s_filename, SchemeObject* s_thunk) {
    assert_arg_type("with-output-to-file", 1, s_string_p, s_filename);
    assert_arg_type("with-output-to-file", 2, s_procedure_p, s_thunk);
    SchemeOutputPort* saved_output_port = current_output_port;
    current_output_port = s_open_output_file(s_filename);
    SchemeObject* result = interpreter->call_procedure_0(s_thunk);
    s_close_output_port(current_output_port);
    current_output_port = saved_output_port;
    return result;
}

SchemeObject* s_with_input_from_file(SchemeObject* s_filename, SchemeObject* s_thunk) {
    assert_arg_type("with-input-from-file", 1, s_string_p, s_filename);
    assert_arg_type("with-input-from-file", 2, s_procedure_p, s_thunk);
    SchemeInputPort* saved_input_port = current_input_port;
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
        is = static_cast<SchemeInputPort*>(s_port)->is;
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
        is = static_cast<SchemeInputPort*>(s_port)->is;
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
        os = static_cast<SchemeOutputPort*>(port)->os;
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
        is = static_cast<SchemeInputPort*>(s_port)->is;
    }
    return global_parser->read(is);
}


SchemeObject* s_write(SchemeObject* o, SchemeObject* port) {
    ostream* os;
    if (port == S_UNSPECIFIED) {
        os = s_current_output_port()->os;
    } else {
        assert_arg_type("write", 2, s_output_port_p, port);
        os = static_cast<SchemeOutputPort*>(port)->os;
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
        os = static_cast<SchemeOutputPort*>(port)->os;
    }
    
    if (s_string_p(o) == S_TRUE) {
        (*os) << static_cast<SchemeString*>(o)->str;
    } else if (s_char_p(o) == S_TRUE) {
        (*os) << static_cast<SchemeChar*>(o)->c;
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
        os = static_cast<SchemeOutputPort*>(port)->os;
    }
    (*os) << endl;        
    return S_UNSPECIFIED;
}

SchemeBool* s_environment_p(SchemeObject* o) {
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
        throw scheme_exception("Not a valid version. Only 5 is supported.");
    }
    return scheme_report_environment;
}

SchemeObject* s_interaction_environment(SchemeObject* s_version) {
    assert_arg_type("interaction-environment", 1, s_integer_p, s_version);
    if (scm2int(s_version) != 5) {
        throw scheme_exception("Not a valid version. Only 5 is supported.");
    }
    return interaction_environment;
}

SchemeObject* s_eval(SchemeObject* expression, SchemeObject* s_environment) {
    assert_arg_type("eval", 2, s_environment_p, s_environment);
    SchemeEnvironment* environment = static_cast<SchemeEnvironment*>(s_environment);
    SchemeObject* expressions = s_cons(expression, S_EMPTY_LIST);
    Interpreter interpreter = Interpreter(expressions, environment);
    return interpreter.interpret();
}

