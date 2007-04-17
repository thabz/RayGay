
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
SchemeNumber* S_ZERO = new SchemeNumber(0);
SchemeNumber* S_ONE = new SchemeNumber(1);
SchemeNumber* S_TWO = new SchemeNumber(2);
SchemeNumber* S_THREE = new SchemeNumber(3);
SchemeNumber* S_FOUR = new SchemeNumber(4);
SchemeNumber* S_FIVE = new SchemeNumber(5);
SchemeNumber* S_SIX = new SchemeNumber(6);
SchemeNumber* S_SEVEN = new SchemeNumber(7);
SchemeNumber* S_EIGHT = new SchemeNumber(8);
SchemeNumber* S_NINE = new SchemeNumber(9);
SchemeUnspecified* S_UNSPECIFIED = new SchemeUnspecified();
SchemeEmptyList* S_EMPTY_LIST = new SchemeEmptyList();
SchemeChar* S_SPACE = new SchemeChar(' ');
SchemeNumber* S_NUMBERS[] = {S_ZERO, S_ONE, S_TWO, S_THREE, S_FOUR, S_FIVE, S_SIX, S_SEVEN, S_EIGHT, S_NINE};

SchemeInputPort* current_input_port = NULL;
SchemeOutputPort* current_output_port = NULL;

Interpreter* interpreter;

Scheme::Scheme() {
    top_level_bindings = new BindingEnvironment(NULL);
	assign("apply"      ,1,0,1, (SchemeObject* (*)()) s_apply);
	assign("map"        ,1,0,1, (SchemeObject* (*)()) s_map);
	assign("for-each"   ,1,0,1, (SchemeObject* (*)()) s_for_each);
	assign("equal?"     ,2,0,0, (SchemeObject* (*)()) s_equal_p);
	assign("eq?"        ,2,0,0, (SchemeObject* (*)()) s_eq_p);
	assign("eqv?"       ,2,0,0, (SchemeObject* (*)()) s_eqv_p);
	assign("boolean?"   ,1,0,0, (SchemeObject* (*)()) s_boolean_p);
	assign("pair?"      ,1,0,0, (SchemeObject* (*)()) s_pair_p);
	assign("symbol?"    ,1,0,0, (SchemeObject* (*)()) s_symbol_p);
	assign("char?"      ,1,0,0, (SchemeObject* (*)()) s_char_p);
	assign("list?"      ,1,0,0, (SchemeObject* (*)()) s_list_p);
	assign("string?"    ,1,0,0, (SchemeObject* (*)()) s_string_p);
	assign("procedure?" ,1,0,0, (SchemeObject* (*)()) s_procedure_p);
	assign("number?"    ,1,0,0, (SchemeObject* (*)()) s_number_p);
	assign("integer?"   ,1,0,0, (SchemeObject* (*)()) s_integer_p);
	assign("complex?"   ,1,0,0, (SchemeObject* (*)()) s_complex_p);
	assign("real?"      ,1,0,0, (SchemeObject* (*)()) s_real_p);
	assign("rational?"  ,1,0,0, (SchemeObject* (*)()) s_rational_p);
	assign("exact?"     ,1,0,0, (SchemeObject* (*)()) s_exact_p);
	assign("inexact?"   ,1,0,0, (SchemeObject* (*)()) s_inexact_p);
	assign("vector?"    ,1,0,0, (SchemeObject* (*)()) s_vector_p);
	assign("null?"      ,1,0,0, (SchemeObject* (*)()) s_null_p);
	assign("car"        ,1,0,0, (SchemeObject* (*)()) s_car);
	assign("cdr"        ,1,0,0, (SchemeObject* (*)()) s_cdr);
	assign("cadr"       ,1,0,0, (SchemeObject* (*)()) s_cadr);
	assign("cdar"       ,1,0,0, (SchemeObject* (*)()) s_cdar);
	assign("list"       ,0,0,1, (SchemeObject* (*)()) s_list);
	assign("list-tail"  ,2,0,0, (SchemeObject* (*)()) s_list_tail);
	assign("list-ref"   ,2,0,0, (SchemeObject* (*)()) s_list_ref);
	assign("set-car!"   ,2,0,0, (SchemeObject* (*)()) s_set_car_e);
	assign("set-cdr!"   ,2,0,0, (SchemeObject* (*)()) s_set_cdr_e);
	assign("assoc"      ,2,0,0, (SchemeObject* (*)()) s_assoc);
	assign("assq"       ,2,0,0, (SchemeObject* (*)()) s_assq);
	assign("assv"       ,2,0,0, (SchemeObject* (*)()) s_assv);
	assign("append"     ,0,0,1, (SchemeObject* (*)()) s_append);
	assign("member"     ,2,0,0, (SchemeObject* (*)()) s_member);
	assign("memq"       ,2,0,0, (SchemeObject* (*)()) s_memq);
	assign("memv"       ,2,0,0, (SchemeObject* (*)()) s_memv);
	assign("reverse"    ,1,0,0, (SchemeObject* (*)()) s_reverse);
	assign("length"     ,1,0,0, (SchemeObject* (*)()) s_length);
	assign("cons"       ,2,0,0, (SchemeObject* (*)()) s_cons);
	assign("display"    ,1,1,0, (SchemeObject* (*)()) s_display);
	assign("write"      ,1,1,0, (SchemeObject* (*)()) s_write);
	assign("newline"    ,0,1,0, (SchemeObject* (*)()) s_newline);
	assign("<"          ,0,0,1, (SchemeObject* (*)()) s_less);
	assign(">"          ,0,0,1, (SchemeObject* (*)()) s_greater);
	assign("<="         ,0,0,1, (SchemeObject* (*)()) s_less_equal);
	assign(">="         ,0,0,1, (SchemeObject* (*)()) s_greater_equal);
	assign("="          ,0,0,1, (SchemeObject* (*)()) s_equal);
	assign("+"          ,0,0,1, (SchemeObject* (*)()) s_plus);
	assign("-"          ,0,0,1, (SchemeObject* (*)()) s_minus);
	assign("*"          ,0,0,1, (SchemeObject* (*)()) s_mult);
	assign("/"          ,0,0,1, (SchemeObject* (*)()) s_divide);
	assign("abs"        ,1,0,0, (SchemeObject* (*)()) s_abs);
	assign("tan"        ,1,0,0, (SchemeObject* (*)()) s_tan);
	assign("atan"       ,1,1,0, (SchemeObject* (*)()) s_atan);
	assign("sin"        ,1,0,0, (SchemeObject* (*)()) s_sin);
	assign("asin"       ,1,0,0, (SchemeObject* (*)()) s_asin);
	assign("cos"        ,1,0,0, (SchemeObject* (*)()) s_cos);
	assign("acos"       ,1,0,0, (SchemeObject* (*)()) s_acos);
	assign("sqrt"       ,1,0,0, (SchemeObject* (*)()) s_sqrt);
	assign("log"        ,1,0,0, (SchemeObject* (*)()) s_log);
	assign("exp"        ,1,0,0, (SchemeObject* (*)()) s_exp);
	assign("expt"       ,2,0,0, (SchemeObject* (*)()) s_expt);
	assign("round"      ,1,0,0, (SchemeObject* (*)()) s_round);
	assign("ceiling"    ,1,0,0, (SchemeObject* (*)()) s_ceiling);
	assign("floor"      ,1,0,0, (SchemeObject* (*)()) s_floor);
	assign("truncate"   ,1,0,0, (SchemeObject* (*)()) s_truncate);
	assign("quotient"   ,2,0,0, (SchemeObject* (*)()) s_quotient);
	assign("remainder"  ,2,0,0, (SchemeObject* (*)()) s_remainder);
	assign("modulo"     ,2,0,0, (SchemeObject* (*)()) s_modulo);
	assign("min"        ,1,0,1, (SchemeObject* (*)()) s_min);
	assign("max"        ,1,0,1, (SchemeObject* (*)()) s_max);
	assign("gcd"        ,0,0,1, (SchemeObject* (*)()) s_gcd);
	assign("lcm"        ,0,0,1, (SchemeObject* (*)()) s_lcm);
	assign("even?"      ,1,0,1, (SchemeObject* (*)()) s_even_p);
	assign("odd?"       ,1,0,1, (SchemeObject* (*)()) s_odd_p);
	assign("zero?"      ,1,0,0, (SchemeObject* (*)()) s_zero_p);
	assign("negative?"  ,1,0,0, (SchemeObject* (*)()) s_negative_p);
	assign("positive?"  ,1,0,0, (SchemeObject* (*)()) s_positive_p);
	assign("not"        ,1,0,0, (SchemeObject* (*)()) s_not);
	assign("make-vector",1,1,0, (SchemeObject* (*)()) s_make_vector);
	assign("vector"     ,0,0,1, (SchemeObject* (*)()) s_vector);
	assign("vector-length",1,0,0, (SchemeObject* (*)()) s_vector_length);
	assign("vector-ref" ,2,0,0, (SchemeObject* (*)()) s_vector_ref);
	assign("vector-set!",3,0,0, (SchemeObject* (*)()) s_vector_set_e);
	assign("vector-fill!",2,0,0, (SchemeObject* (*)()) s_vector_fill_e);
	assign("list->vector",1,0,0, (SchemeObject* (*)()) s_list_2_vector);
	assign("vector->list",1,0,0, (SchemeObject* (*)()) s_vector_2_list);
	assign("make-string" ,1,1,0, (SchemeObject* (*)()) s_make_string);
	assign("string"      ,0,0,1, (SchemeObject* (*)()) s_string);
	assign("string-length",1,0,0, (SchemeObject* (*)()) s_string_length);
	assign("string-ref"  ,2,0,0, (SchemeObject* (*)()) s_string_ref);
	assign("string-set!"  ,3,0,0, (SchemeObject* (*)()) s_string_set_e);
	assign("string-append",0,0,1, (SchemeObject* (*)()) s_string_append);
	assign("string-copy",1,0,0, (SchemeObject* (*)()) s_string_copy);
	assign("substring"    ,3,0,0, (SchemeObject* (*)()) s_substring);
	assign("symbol->string",1,0,0, (SchemeObject* (*)()) s_symbol_2_string);
	assign("string->symbol",1,0,0, (SchemeObject* (*)()) s_string_2_symbol);
	assign("char->integer",1,0,0, (SchemeObject* (*)()) s_char_2_integer);
	assign("integer->char",1,0,0, (SchemeObject* (*)()) s_integer_2_char);
	assign("number->string",1,1,0, (SchemeObject* (*)()) s_number_2_string);
	assign("string->number",1,1,0, (SchemeObject* (*)()) s_string_2_number);
	assign("list->string",1,0,0, (SchemeObject* (*)()) s_list_2_string);
	assign("string->list",1,0,0, (SchemeObject* (*)()) s_string_2_list);
	assign("char-downcase",1,0,0, (SchemeObject* (*)()) s_char_downcase);
	assign("char-upcase" ,1,0,0, (SchemeObject* (*)()) s_char_upcase);
	assign("char-alphabetic?" ,1,0,0, (SchemeObject* (*)()) s_char_alphabetic_p);
	assign("char-numeric?" ,1,0,0, (SchemeObject* (*)()) s_char_numeric_p);
	assign("char-whitespace?" ,1,0,0, (SchemeObject* (*)()) s_char_whitespace_p);
	assign("char-upper-case?" ,1,0,0, (SchemeObject* (*)()) s_char_upper_case_p);
	assign("char-lower-case?" ,1,0,0, (SchemeObject* (*)()) s_char_lower_case_p);
	assign("symgen"      ,0,0,0, (SchemeObject* (*)()) s_symgen);
	
	assign("current-input-port"    ,0,0,0, (SchemeObject* (*)()) s_current_input_port);
	assign("current-output-port"   ,0,0,0, (SchemeObject* (*)()) s_current_output_port);
	assign("input-port?"           ,1,0,0, (SchemeObject* (*)()) s_input_port_p);
	assign("output-port?"          ,1,0,0, (SchemeObject* (*)()) s_output_port_p);
	assign("eof-object?"           ,1,0,0, (SchemeObject* (*)()) s_eof_object_p);
	assign("call-with-input-file"  ,2,0,0, (SchemeObject* (*)()) s_call_with_input_file);
	assign("call-with-output-file" ,2,0,0, (SchemeObject* (*)()) s_call_with_output_file);
	assign("with-input-from-file"  ,2,0,0, (SchemeObject* (*)()) s_with_input_from_file);
	assign("with-output-to-file"   ,2,0,0, (SchemeObject* (*)()) s_with_output_to_file);
	assign("open-input-file"       ,1,0,0, (SchemeObject* (*)()) s_open_input_file);
	assign("open-output-file"      ,1,0,0, (SchemeObject* (*)()) s_open_output_file);
	assign("close-input-port"      ,1,0,0, (SchemeObject* (*)()) s_close_input_port);
	assign("close-output-port"     ,1,0,0, (SchemeObject* (*)()) s_close_output_port);
	assign("read-char"             ,0,1,0, (SchemeObject* (*)()) s_read_char);
	assign("peek-char"             ,0,1,0, (SchemeObject* (*)()) s_peek_char);
	
    current_input_port = new SchemeInputPort(&cin);
    current_output_port = new SchemeOutputPort(&cout);
	
    ifstream infile;
    infile.open("init.scm", ifstream::in);
    eval(&infile);
    infile.close();
}

SchemeObject* Scheme::eval(istream* is) {
    Lexer* lexer = new Lexer(is);
    Parser* parser = new Parser(lexer);
    SchemePair* parse_tree = parser->parse();
    interpreter = new Interpreter(parse_tree, top_level_bindings);
    return interpreter->interpret();
}

SchemeObject* Scheme::eval(string data) {
    istream* is = new istringstream(data);
    SchemeObject* result = eval(is);
    delete is;
    return result;
};

void Scheme::assign(string variable, int req, int opt, int rst, SchemeObject* (*fn)()) {
    SchemeSymbol* name = SchemeSymbol::create(variable);
    SchemeProcedure* proc = new SchemeProcedure(name, req, opt, rst, fn);
    top_level_bindings->put(name, proc);
}

// -----------------------------------------------------
// Procedures
// -----------------------------------------------------

inline
void assert_arg_type(char* procname, int argnum, SchemeBool* (*test_fn)(SchemeObject*), SchemeObject* arg) {
    if ((*test_fn)(arg) == S_FALSE) {
        throw scheme_exception("Wrong argument-type in position in call to " + string(procname));
    }
}


inline
SchemeNumber* make_number(int n) {
    if (n < 10 && n >= 0) {
        return S_NUMBERS[n];
    }
    return new SchemeNumber(n);
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
        if (s_pair_p(fast) == S_FALSE) return fast == S_EMPTY_LIST ? S_TRUE : S_FALSE;
        fast = s_cdr(fast);
        if (slow == fast) {
            // The fast stepping pointer has looped around and caught up with the slow
            // moving pointer, thus the structure is circular and thus not a list.
            return S_FALSE;
        }
        if (s_pair_p(fast) == S_FALSE) return fast == S_EMPTY_LIST ? S_TRUE : S_FALSE;
        fast = s_cdr(fast);
        slow = s_cdr(slow);
        if (slow == fast) {
            // The fast stepping pointer has looped around and caught up with the slow
            // moving pointer, thus the structure is circular and thus not a list.
            return S_FALSE;
        }
    }
}

// args is a list (arg1 arg2 ... argn). argn must be a list. proc is called with the arguments
// (append (list arg1 arg2 ...) argn)
SchemeObject* s_apply(SchemeObject* proc, SchemeObject* args) {
    assert_arg_type("apply", 1, s_procedure_p, proc);

    SchemePair* collected = S_EMPTY_LIST;
    SchemePair* prev = NULL;
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

    return interpreter->call_procedure_n(proc,collected);
}

SchemeObject* s_map(SchemeObject* proc, SchemeObject* lists) {
    assert_arg_type("map", 1, s_procedure_p, proc);
    
    SchemePair* result = S_EMPTY_LIST;
    SchemeObject* prev = S_EMPTY_LIST;

    // Vi skralder af lists i hvert gennemløb. Så ((1 2 3)(10 20 30)) bliver til ((2 3)(20 30)) og til sidst ((3)(30))
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
    
    return result;    
}

SchemeObject* s_for_each(SchemeObject* proc, SchemeObject* lists) {
    s_map(proc, lists);
    return S_UNSPECIFIED;
}


SchemeObject* member_helper(SchemeBool* (comparator)(SchemeObject*,SchemeObject*), SchemeObject* obj, SchemeObject* p) {
    while (s_null_p(p) == S_FALSE) {
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

SchemePair* s_list_tail(SchemePair* l, SchemeNumber* k) {
    int i = int(k->number);
    if (i < 0) {
        throw scheme_exception("Index out of range: " + k->toString());
    }
    while (i-- > 0) {
        if (l == S_EMPTY_LIST) {
            throw scheme_exception("Index out of range: " + k->toString());
        }
        l = l->cdrAsPair();
    }
    //if (i != 0) {
    //    // Catches SchemeNumber being a non-integer
    //    throw scheme_exception("Index out of range: " + k->toString());
    //}
    return l;
}

SchemeObject* s_list_ref(SchemePair* l, SchemeNumber* index) {
    SchemePair* p = s_list_tail(l, index);
    if (p == S_EMPTY_LIST) {
        throw scheme_exception("Index out of range: " + index->toString());
    }
    return p->car;
}

SchemeObject* assoc_helper(SchemeBool* (comparator)(SchemeObject*,SchemeObject*), SchemeObject* obj, SchemePair* alist) {
    while (alist != S_EMPTY_LIST) {
        if (s_pair_p(alist->car) == S_FALSE) {
            throw scheme_exception("Illegal argument");
        }
        SchemePair* p = static_cast<SchemePair*>(alist->car);
        if ((*comparator)(obj, p->car) == S_TRUE) {
            return p;
        }
        alist = alist->cdrAsPair();
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
SchemeBool* s_pair_p(SchemeObject* p) {
    return (p->type() == SchemeObject::PAIR) ? S_TRUE : S_FALSE;
}

// (symbol? p)
SchemeBool* s_symbol_p(SchemeObject* p) {
    return (p->type() == SchemeObject::SYMBOL) ? S_TRUE : S_FALSE;
}

// (string? p)
SchemeBool* s_string_p(SchemeObject* p) {
    return (p->type() == SchemeObject::STRING) ? S_TRUE : S_FALSE;
}

// (procedure? p)
SchemeBool* s_procedure_p(SchemeObject* p) {
    return (p->type() == SchemeObject::PROCEDURE) ? S_TRUE : S_FALSE;
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
    return (p->type() == SchemeObject::EMPTY_LIST) ? S_TRUE : S_FALSE;
}


SchemeObject* s_car(SchemeObject* o) {
    assert_arg_type("car", 1, s_pair_p, o);
    return static_cast<SchemePair*>(o)->car;
}

SchemeObject* s_cdr(SchemeObject* o) {
    assert_arg_type("cdr", 1, s_pair_p, o);
    return static_cast<SchemePair*>(o)->cdr;
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

// (cons a b)
SchemePair* s_cons(SchemeObject* car, SchemeObject* cdr) {
    return new SchemePair(car, cdr);
}

SchemePair* s_list(SchemePair* args) {
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
    static_cast<SchemePair*>(p)->car = o;
    return S_UNSPECIFIED;
}

SchemeObject* s_set_cdr_e(SchemeObject* p, SchemeObject* o) {
    assert_arg_type("set-cdr!", 1, s_pair_p, p);
    static_cast<SchemePair*>(p)->cdr = o;
    return S_UNSPECIFIED;
}

SchemeObject* s_append(SchemePair* p) {
    SchemePair* result = S_EMPTY_LIST;
    SchemePair* prev = NULL;
    SchemePair* tmp;
    while (p != S_EMPTY_LIST) {
        if (p->car == S_EMPTY_LIST) {
            // Skip empty lists
    	    p = p->cdrAsPair();
            continue;
        }
	    if (p->car->type() == SchemeObject::PAIR && p->cdr != S_EMPTY_LIST) {
    	    SchemePair* pp = static_cast<SchemePair*>(p->car);
    	    while (pp->type() == SchemeObject::PAIR) {
    	        if (result == S_EMPTY_LIST) {
                    result = s_cons(pp->car, S_EMPTY_LIST);
                    prev = result;
    	        } else {
    	            tmp = s_cons(pp->car, S_EMPTY_LIST);
                    prev->cdr = tmp;
                    prev = tmp;
	            }
	            pp = pp->cdrAsPair();
    	    }
    	    p = p->cdrAsPair();
	    } else if (p->car->type() == SchemeObject::PAIR && p->cdr == S_EMPTY_LIST) {
	        // Appends final list whether proper or inproper
            prev->cdr = p->car;
            return result;
	    } else {
	        if (p->cdr != S_EMPTY_LIST) {
	            throw scheme_exception("Only last argument can be a nonproper list");
            } else {
                if (result == S_EMPTY_LIST) {
                    return p->car;
                } else {
                    prev->cdr = p->cdr;
                    return result;
                }
            }
	    }
    }
    if (result != S_EMPTY_LIST) {
        prev->cdr = S_EMPTY_LIST;
    }
    return result;
}

SchemeNumber* s_plus(SchemePair* p) {
	double result = 0;
    int i = 1;
	while (p != S_EMPTY_LIST) {
	    assert_arg_type("+", i++, s_number_p, p->car);
		SchemeNumber* n = static_cast<SchemeNumber*>(p->car);
		result += n->number;
		p = p->cdrAsPair();
	}
	return new SchemeNumber(result);
}

SchemeNumber* s_minus(SchemePair* p) {
	double result = 0;
    double first = true;
	if (p == S_EMPTY_LIST) {
		throw scheme_exception("Wrong number of arguments");
	}
	if (p->cdr == S_EMPTY_LIST) {
	    // One-argument case is a simple negate (n => -n)
        first = false;
	}
    int i = 1;
	while (p != S_EMPTY_LIST) {
	    assert_arg_type("-", i++, s_number_p, p->car);
		SchemeNumber* n = static_cast<SchemeNumber*>(p->car);
		if (first) {
            result = n->number;
            first = false;
		} else {
		    result -= n->number;
	    }
		p = p->cdrAsPair();
	}
	return new SchemeNumber(result);
}

SchemeNumber* s_divide(SchemePair* p) {
	double result = 1;
    double first = true;
	if (p == S_EMPTY_LIST) {
		throw scheme_exception("Wrong number of arguments");
	}
	if (p->cdr == S_EMPTY_LIST) {
	    // One-argument case is a simple inverse (n => 1/n)
        first = false;
	}
    int i = 1;
	while (p != S_EMPTY_LIST) {
	    assert_arg_type("/", i++, s_number_p, p->car);
		SchemeNumber* n = static_cast<SchemeNumber*>(p->car);
		if (first) {
            result = n->number;
            first = false;
		} else {
		    result /= n->number;
	    }
		p = p->cdrAsPair();
	}
	return new SchemeNumber(result);
}


SchemeNumber* s_mult(SchemeObject* p) {
	double result = 1;
    int i = 1;
	while (p != S_EMPTY_LIST) {
	    assert_arg_type("*", i++, s_number_p, s_car(p));
		SchemeNumber* n = static_cast<SchemeNumber*>(s_car(p));
		result *= n->number;
		p = s_cdr(p);
	}
	return new SchemeNumber(result);
}

SchemeVector* s_make_vector(SchemeNumber* count, SchemeObject* obj) {
    int c = int(count->number);
    return new SchemeVector(obj, c);
}

SchemeVector* s_vector(SchemePair* args) {
    int c = int(s_length(args)->number);
    SchemeObject** elems = new SchemeObject*[c];
    int i = 0;
    while (args != S_EMPTY_LIST) {
        elems[i++] = args->car;
        args = args->cdrAsPair();
    }
    return new SchemeVector(elems,c);
}

SchemeNumber* s_vector_length(SchemeObject* v) {
    if (s_vector_p(v) == S_FALSE) {
        throw scheme_exception("Not a vector");
    }
    SchemeVector* vv = static_cast<SchemeVector*>(v);
    return make_number(vv->length);
}

SchemeVector* s_list_2_vector(SchemeObject* list) {
    if (s_list_p(list) == S_FALSE) {
        throw scheme_exception("list->vector wrong type in argument 1");
    }
    return s_vector(static_cast<SchemePair*>(list));
}

SchemePair* s_vector_2_list(SchemeObject* o) {
    if (s_vector_p(o) == S_FALSE) {
        throw scheme_exception("Not a vector");
    }
    SchemeVector* v = static_cast<SchemeVector*>(o);
    SchemePair* result = S_EMPTY_LIST;
    for(int i = v->length-1; i >= 0; i--) {
	    result = s_cons(v->get(i), result);
    }
    return result;
}

SchemeObject* s_vector_ref(SchemeVector* v, SchemeNumber* index) {
    int i = int(index->number);
    return v->get(i);
}

SchemeObject* s_vector_set_e(SchemeVector* vec, SchemeNumber* index, SchemeObject* val) {
    int i = int(index->number);
    vec->set(val,i);
    return S_UNSPECIFIED;
}

SchemeObject* s_vector_fill_e(SchemeVector* v, SchemeObject* fill) {
    for(int i = 0; i < v->length; i++) {
	    v->set(fill,i);
    }
    return S_UNSPECIFIED;
}


SchemeNumber* s_sqrt(SchemeNumber* n) {
    return new SchemeNumber(sqrt(n->number));
}

SchemeNumber* s_abs(SchemeNumber* n) {
    return new SchemeNumber(fabs(n->number));
}


SchemeNumber* s_sin(SchemeNumber* n) {
    return new SchemeNumber(sin(n->number));
}

SchemeNumber* s_asin(SchemeNumber* n) {
    return new SchemeNumber(asin(n->number));
}

SchemeNumber* s_cos(SchemeNumber* n) {
    return new SchemeNumber(cos(n->number));
}

SchemeNumber* s_acos(SchemeNumber* n) {
    return new SchemeNumber(acos(n->number));
}

SchemeNumber* s_tan(SchemeNumber* n) {
    return new SchemeNumber(tan(n->number));
}

SchemeNumber* s_atan(SchemeNumber* y, SchemeObject* x) {
    if (x == S_UNSPECIFIED) {
        return new SchemeNumber(atan(y->number));
    } else {
        SchemeNumber* xx = static_cast<SchemeNumber*>(x);
        return new SchemeNumber(atan2(y->number, xx->number));
    }
}

SchemeNumber* s_log(SchemeNumber* n) {
    return new SchemeNumber(log(n->number));
}

// Returns a^b
SchemeNumber* s_expt(SchemeNumber* a, SchemeNumber* b) {
    return new SchemeNumber(pow(a->number,b->number));
}

// Returns e^n
SchemeNumber* s_exp(SchemeNumber* n) {
    return new SchemeNumber(exp(n->number));
}

// Round returns the closest integer to x, rounding to even when x is halfway between two integers.
SchemeNumber* s_round(SchemeObject* n) {
    assert_arg_type("round", 1, s_number_p, n);
    double nn = static_cast<SchemeNumber*>(n)->number;
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
    return new SchemeNumber(result);
}

// Ceiling returns the smallest integer not smaller than x
SchemeNumber* s_ceiling(SchemeObject* n) {
    assert_arg_type("ceiling", 1, s_number_p, n);
    return new SchemeNumber(ceil(static_cast<SchemeNumber*>(n)->number));
}

// Floor returns the largest integer not larger than x
SchemeNumber* s_floor(SchemeObject* n) {
    assert_arg_type("floor", 1, s_number_p, n);
    return new SchemeNumber(floor(static_cast<SchemeNumber*>(n)->number));
}

// Truncate returns the integer closest to x whose absolute value is not larger than the absolute value of x
SchemeNumber* s_truncate(SchemeObject* n) {
    assert_arg_type("truncate", 1, s_number_p, n);
    return new SchemeNumber(trunc(static_cast<SchemeNumber*>(n)->number));
}

SchemeNumber* s_quotient(SchemeObject* n1, SchemeObject* n2) {
    assert_arg_type("quotient", 1, s_integer_p, n1);
    assert_arg_type("quotient", 2, s_integer_p, n2);
    int nn1 = int(static_cast<SchemeNumber*>(n1)->number);
    int nn2 = int(static_cast<SchemeNumber*>(n2)->number);
    return make_number(nn1 / nn2);
    
}

SchemeNumber* s_remainder(SchemeObject* n1, SchemeObject* n2) {
    assert_arg_type("remainder", 1, s_integer_p, n1);
    assert_arg_type("remainder", 2, s_integer_p, n2);
    int nn1 = int(static_cast<SchemeNumber*>(n1)->number);
    int nn2 = int(static_cast<SchemeNumber*>(n2)->number);
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
    int nn1 = int(static_cast<SchemeNumber*>(n1)->number);
    int nn2 = int(static_cast<SchemeNumber*>(n2)->number);
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


SchemeNumber* s_min(SchemeNumber* first, SchemePair* rest) {
    SchemeNumber* result = first;
    int i = 2;    
	while (rest != S_EMPTY_LIST) {
	    assert_arg_type("min", i++, s_number_p, rest->car);
    	SchemeNumber* n = static_cast<SchemeNumber*>(rest->car);
        result = n->number < result->number ? n : result;
        rest = rest->cdrAsPair();
	}
	return result;
}


SchemeNumber* s_max(SchemeNumber* first, SchemePair* rest) {
    SchemeNumber* result = first;
    int i = 2;    
	while (rest != S_EMPTY_LIST) {
	    assert_arg_type("max", i++, s_number_p, rest->car);
    	SchemeNumber* n = static_cast<SchemeNumber*>(rest->car);
        result = n->number > result->number ? n : result;
        rest = rest->cdrAsPair();
	}
	return result;
}

int gcd(int a, int b) {
    int t = 0;
    while(b != 0) {
        t = b;
        b = a % b;
        a = t;
    }
    return t;
}

// Using Euclids algorithm and that gcd is associative thus gcd(a,b,c) = gcd(a,(gcd(b,c))) = gcd(gcd(a,b),c).
SchemeNumber* s_gcd(SchemeObject* l) {
    if (s_null_p(l) == S_TRUE) {
        return S_ZERO;
    };
    assert_arg_type("gcd", 1, s_pair_p, l);
    assert_arg_type("gcd", 1, s_integer_p, s_car(l));
    if (s_null_p(s_cdr(l)) == S_TRUE) {
        return make_number(abs(int(static_cast<SchemeNumber*>(s_car(l))->number)));
    }
    int a = abs(int(static_cast<SchemeNumber*>(s_car(l))->number));
    int b = abs(int(static_cast<SchemeNumber*>(s_gcd(s_cdr(l)))->number));
    return make_number(gcd(a,b));
}

// Using the property gcd(a,b) * lcm(a,b) = a * b and that lcm(a,b,c) = lcm(lcm(a,b),c) = lcm(a,lcm(b,c))
SchemeNumber* s_lcm(SchemeObject* l) {
    if (s_null_p(l) == S_TRUE) {
        return S_ONE;
    }
    assert_arg_type("lcm", 1, s_pair_p, l);
    if (s_null_p(s_cdr(l)) == S_TRUE) {
        assert_arg_type("lcm", 1, s_integer_p, s_car(l));
        return make_number(abs(int(static_cast<SchemeNumber*>(s_car(l))->number)));
    }

    int a = abs(int(static_cast<SchemeNumber*>(s_car(l))->number));
    int b = abs(int(static_cast<SchemeNumber*>(s_lcm(s_cdr(l)))->number));
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
    assert_arg_type("even?", 1, s_number_p, n);
    int nn = int(static_cast<SchemeNumber*>(n)->number);
    return (nn % 2 == 0) ? S_TRUE : S_FALSE;
}

SchemeBool* s_odd_p(SchemeObject* n) {
    assert_arg_type("odd?", 1, s_number_p, n);
    int nn = int(static_cast<SchemeNumber*>(n)->number);
    return (abs(nn % 2) == 1) ? S_TRUE : S_FALSE;
}

SchemeBool* s_zero_p(SchemeObject* n) {
    assert_arg_type("zero?", 1, s_number_p, n);
    return static_cast<SchemeNumber*>(n)->number == 0 ? S_TRUE : S_FALSE;
}
SchemeBool* s_negative_p(SchemeObject* n) {
    assert_arg_type("negative?", 1, s_number_p, n);
    return static_cast<SchemeNumber*>(n)->number < 0 ? S_TRUE : S_FALSE;
}

SchemeBool* s_positive_p(SchemeObject* n) {
    assert_arg_type("position?", 1, s_number_p, n);
    return static_cast<SchemeNumber*>(n)->number > 0 ? S_TRUE : S_FALSE;
}

SchemeBool* s_equal(SchemePair* p) {
    if (p == S_EMPTY_LIST) {
        return S_TRUE;
    }
    if (p->car->type() != SchemeObject::NUMBER) {
        throw scheme_exception("Wrong argument to <");
    }
    
    double n = static_cast<SchemeNumber*>(p->car)->number;
    p = p->cdrAsPair();
    while (p != S_EMPTY_LIST) {
        double nn = static_cast<SchemeNumber*>(p->car)->number;
        if (nn != n) {
            return S_FALSE;
        }
        p = p->cdrAsPair();
    }
    return S_TRUE;
}

SchemeBool* s_less(SchemePair* p) {
    if (p == S_EMPTY_LIST) {
        return S_TRUE;
    }
    if (p->car->type() != SchemeObject::NUMBER) {
        throw scheme_exception("Wrong argument to <");
    }
    double n = static_cast<SchemeNumber*>(p->car)->number;
    p = p->cdrAsPair();
    while (p != S_EMPTY_LIST) {
        double nn = static_cast<SchemeNumber*>(p->car)->number;
        if (nn <= n) {
            return S_FALSE;
        }
        n = nn;
        p = p->cdrAsPair();
    }
    return S_TRUE;
}

SchemeBool* s_greater(SchemePair* p) {
    if (p == S_EMPTY_LIST) {
        return S_TRUE;
    }
    if (p->car->type() != SchemeObject::NUMBER) {
        throw scheme_exception("Wrong argument to <");
    }
    double n = static_cast<SchemeNumber*>(p->car)->number;
    p = p->cdrAsPair();
    while (p != S_EMPTY_LIST) {
        double nn = static_cast<SchemeNumber*>(p->car)->number;
        if (nn >= n) {
            return S_FALSE;
        }
        n = nn;
        p = p->cdrAsPair();
    }
    return S_TRUE;
}

SchemeBool* s_less_equal(SchemePair* p) {
    if (p == S_EMPTY_LIST) {
        return S_TRUE;
    }
    if (p->car->type() != SchemeObject::NUMBER) {
        throw scheme_exception("Wrong argument to <");
    }
    double n = static_cast<SchemeNumber*>(p->car)->number;
    p = p->cdrAsPair();
    while (p != S_EMPTY_LIST) {
        double nn = static_cast<SchemeNumber*>(p->car)->number;
        if (nn < n) {
            return S_FALSE;
        }
        n = nn;
        p = p->cdrAsPair();
    }
    return S_TRUE;
}

SchemeBool* s_greater_equal(SchemePair* p) {
    if (p == S_EMPTY_LIST) {
        return S_TRUE;
    }
    if (p->car->type() != SchemeObject::NUMBER) {
        throw scheme_exception("Wrong argument to <");
    }
    double n = static_cast<SchemeNumber*>(p->car)->number;
    p = p->cdrAsPair();
    while (p != S_EMPTY_LIST) {
        double nn = static_cast<SchemeNumber*>(p->car)->number;
        if (nn > n) {
            return S_FALSE;
        }
        n = nn;
        p = p->cdrAsPair();
    }
    return S_TRUE;
}

SchemeBool* s_not(SchemeObject* o) {
    return o->boolValue() ? S_FALSE : S_TRUE;
}


SchemeString* s_make_string(SchemeObject* len, SchemeObject* chr) {
    assert_arg_type("make-string", 1, s_number_p, len);
    
    if (chr == S_UNSPECIFIED) {
        chr = S_SPACE;
    }
    assert_arg_type("make-string", 2, s_char_p, chr);

    string s = string(int(static_cast<SchemeNumber*>(len)->number), static_cast<SchemeChar*>(chr)->c);
    return new SchemeString(s);
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
    return new SchemeString(s);
}

SchemeNumber* s_string_length(SchemeObject* s) {
    assert_arg_type("string-length", 1, s_string_p, s);
    int len = static_cast<SchemeString*>(s)->str.size();
    return make_number(len);
}

SchemeChar* s_string_ref(SchemeString* s, SchemeNumber* i) {
    assert_arg_type("string-ref", 1, s_string_p, s);
    assert_arg_type("string-ref", 2, s_number_p, i);
    int index = int(i->number);
    return new SchemeChar(s->str[index]);
}	

SchemeObject* s_string_set_e(SchemeObject* s, SchemeObject* i, SchemeObject* chr) {
    assert_arg_type("string-set!", 1, s_string_p, s);
    assert_arg_type("string-set!", 2, s_number_p, i);
    assert_arg_type("string-set!", 3, s_char_p, chr);
    scm2string(s)[scm2int(i)] = scm2char(chr);
    return S_UNSPECIFIED;
}

SchemeString* s_symbol_2_string(SchemeSymbol* symbol) {
    return new SchemeString(symbol->str,true);
}

SchemeSymbol* s_string_2_symbol(SchemeString* s) {
    return SchemeSymbol::create(s->str);
}

SchemeString* s_string_append(SchemePair* strings) {
    string result = "";
    int i = 1;
    while (strings != S_EMPTY_LIST) {
        assert_arg_type("string-append", i++, s_string_p, strings->car);
	    result += static_cast<SchemeString*>(strings->car)->str;
	    strings = strings->cdrAsPair();
    }
    return new SchemeString(result);
}

SchemeString* s_string_copy(SchemeObject* str) {
    assert_arg_type("string-copy", 1, s_string_p, str);
    return new SchemeString(static_cast<SchemeString*>(str)->str);
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
        ss << std::setbase(base) << static_cast<SchemeNumber*>(n)->number;
    }
    return new SchemeString(ss.str());
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
    return new SchemeNumber(d);
}


SchemeChar* s_integer_2_char(SchemeObject* i) {
    assert_arg_type("integer->char", 1, s_number_p, i);
    return new SchemeChar(int(static_cast<SchemeNumber*>(i)->number));
}

SchemeNumber* s_char_2_integer(SchemeObject* c) {
    assert_arg_type("char->integer", 1, s_char_p, c);
    return make_number(int(static_cast<SchemeChar*>(c)->c));
}

SchemePair* s_string_2_list(SchemeObject* s) {
    assert_arg_type("string->list", 1, s_string_p, s);
    string ss = static_cast<SchemeString*>(s)->str;
    SchemePair* result = S_EMPTY_LIST;
    for(uint i = 0; i < ss.size(); i++) {
        result = s_cons(new SchemeChar(ss[i]), result);
    }
    // TODO: Leakage
    return s_reverse(result);
}

SchemeString* s_list_2_string(SchemeObject* p) {
    assert_arg_type("list->string", 1, s_list_p, p);
    string result = "";
    while (s_null_p(p) == S_FALSE) {
        assert_arg_type("list->string", 1, s_char_p, s_car(p));
        result += static_cast<SchemeChar*>(s_car(p))->c;
        p = s_cdr(p);
    }
    return new SchemeString(result);
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
    return new SchemeChar(toupper(static_cast<SchemeChar*>(c)->c));    
}

SchemeChar* s_char_downcase(SchemeObject* c) {
    assert_arg_type("char-downcase", 1, s_char_p, c);
    return new SchemeChar(tolower(scm2char(c)));    
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
