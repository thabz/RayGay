
#include "scheme.h"
#include <sstream>
#include <fstream>
#include <cmath>
#include <cctype>

#include "lexer.h"
#include "parser.h"
#include "interpreter.h"

unsigned long symgen_counter = 10000;

SchemeBool* S_TRUE = new SchemeBool(true);
SchemeBool* S_FALSE = new SchemeBool(false);
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

Scheme::Scheme() {
    top_level_bindings = new BindingEnvironment(NULL);
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
	assign("string-length",1,0,0, (SchemeObject* (*)()) s_string_length);
	assign("string-ref"  ,2,0,0, (SchemeObject* (*)()) s_string_ref);
	assign("string-append",0,0,1, (SchemeObject* (*)()) s_string_append);
	assign("string-copy",1,0,0, (SchemeObject* (*)()) s_string_copy);
	assign("symbol->string",1,0,0, (SchemeObject* (*)()) s_symbol_2_string);
	assign("string->symbol",1,0,0, (SchemeObject* (*)()) s_string_2_symbol);
	assign("char->integer",1,0,0, (SchemeObject* (*)()) s_char_2_integer);
	assign("integer->char",1,0,0, (SchemeObject* (*)()) s_integer_2_char);
	assign("number->string",1,0,0, (SchemeObject* (*)()) s_number_2_string);
	assign("string->number",1,0,0, (SchemeObject* (*)()) s_string_2_number);
	assign("list->string",1,0,0, (SchemeObject* (*)()) s_list_2_string);
	assign("string->list",1,0,0, (SchemeObject* (*)()) s_string_2_list);
	assign("char-downcase",1,0,0, (SchemeObject* (*)()) s_char_downcase);
	assign("char-upcase" ,1,0,0, (SchemeObject* (*)()) s_char_upcase);
	assign("symgen"      ,0,0,0, (SchemeObject* (*)()) s_symgen);
	assign("current-input-port",0,0,0, (SchemeObject* (*)()) s_current_input_port);
	assign("current-output-port",0,0,0, (SchemeObject* (*)()) s_current_output_port);
	assign("input-port?"   ,1,0,0, (SchemeObject* (*)()) s_input_port_p);
	assign("output-port?"  ,1,0,0, (SchemeObject* (*)()) s_output_port_p);
	
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
    Interpreter* interpreter = new Interpreter(parse_tree, top_level_bindings);
    return interpreter->interpret();
}

SchemeObject* Scheme::eval(string data) {
    istream* is = new istringstream(data);
    SchemeObject* result = eval(is);
    delete is;
    return result;
};

void Scheme::assign(string variable, int req, int opt, int rst, SchemeObject* (*fn)()) {
    SchemeProcedure* proc = new SchemeProcedure(req, opt, rst, fn);
    top_level_bindings->put(SchemeSymbol::create(variable), proc);
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
    if (a->type() == SchemeObject::NUMBER && a->type() == SchemeObject::NUMBER) {
        double a_n = static_cast<SchemeNumber*>(a)->number;
        double b_n = static_cast<SchemeNumber*>(b)->number;
        return a_n == b_n ? S_TRUE : S_FALSE;
    } else {
        return a == b ? S_TRUE : S_FALSE;
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
    if (o == S_EMPTY_LIST) {
        return S_TRUE;
    }
    SchemePair* p = static_cast<SchemePair*> (o);
    if (p != NULL) {
        while (s_pair_p(p) == S_TRUE && p != S_EMPTY_LIST) {
            p = p->cdrAsPair();
            if (p == NULL) {
                return S_FALSE;
            }
        }
        return p == S_EMPTY_LIST ? S_TRUE : S_FALSE;    
    } else {
        return S_FALSE;
    }
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


SchemeNumber* s_mult(SchemePair* p) {
	double result = 1;
    int i = 1;
	while (p != S_EMPTY_LIST) {
	    assert_arg_type("*", i++, s_number_p, p->car);
		SchemeNumber* n = static_cast<SchemeNumber*>(p->car);
		result *= n->number;
		p = p->cdrAsPair();
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

SchemeBool* s_even_p(SchemeNumber* n) {
    int nn = int(n->number);
    return (nn % 2 == 0) ? S_TRUE : S_FALSE;
}

SchemeBool* s_odd_p(SchemeNumber* n) {
    int nn = int(n->number);
    return (abs(nn % 2) == 1) ? S_TRUE : S_FALSE;
}

SchemeBool* s_zero_p(SchemeNumber* n) {
    return n->number == 0 ? S_TRUE : S_FALSE;
}
SchemeBool* s_negative_p(SchemeNumber* n) {
    return n->number < 0 ? S_TRUE : S_FALSE;
}

SchemeBool* s_positive_p(SchemeNumber* n) {
    return n->number > 0 ? S_TRUE : S_FALSE;
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

SchemeObject* s_number_2_string(SchemeObject* n) {
    assert_arg_type("number->string", 1, s_number_p, n);
    ostringstream ss;
    ss << static_cast<SchemeNumber*>(n)->number;
    return new SchemeString(ss.str());
    
}

SchemeObject* s_string_2_number(SchemeObject* s) {
    assert_arg_type("string->number", 1, s_string_p, s);
    istream* is = new istringstream(static_cast<SchemeString*>(s)->str);
    double d;
    (*is) >> d;
    delete is;
    return make_number(d);
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


SchemeChar* s_char_upcase(SchemeObject* c) {
    assert_arg_type("char-upcase", 1, s_char_p, c);
    return new SchemeChar(toupper(static_cast<SchemeChar*>(c)->c));    
}

SchemeChar* s_char_downcase(SchemeObject* c) {
    assert_arg_type("char-downcase", 1, s_char_p, c);
    return new SchemeChar(tolower(static_cast<SchemeChar*>(c)->c));    
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


