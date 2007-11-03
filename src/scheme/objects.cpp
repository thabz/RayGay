
#include "objects.h"
#include "scheme.h"
#include <sstream>
#include <iomanip>
#include "heap.h"

// Map of known symbols
map<wstring,SchemeObject*> SchemeObject::known_symbols;

// Sequence for subtype identities
int SchemeObject::subtypes_seq = 1;

//-----------------------------------------------------------
// Static factory methods
//-----------------------------------------------------------

SchemeObject* SchemeObject::createRealNumber(double number) {
    SchemeObject* result = Heap::getUniqueInstance()->allocate(SchemeObject::REAL_NUMBER);
    result->real_value = number;
    return result;
}

SchemeObject* SchemeObject::createComplexNumber(SchemeObject* real, SchemeObject* imag) {
    SchemeObject* result = Heap::getUniqueInstance()->allocate(SchemeObject::COMPLEX_NUMBER);
    result->real = real;
    result->imag = imag;
    return result;
}

SchemeObject* SchemeObject::createComplexNumber(std::complex<double> c) {
    SchemeObject* real = createRealNumber(c.real());
    SchemeObject* imag = createRealNumber(c.imag());
    return createComplexNumber(real, imag);
}

SchemeObject* SchemeObject::createRationalNumber(SchemeObject* numerator, SchemeObject* denominator) {
    SchemeObject* result = Heap::getUniqueInstance()->allocate(SchemeObject::RATIONAL_NUMBER);
    assert(numerator->type() == INTEGER_NUMBER);
    assert(denominator->type() == INTEGER_NUMBER);
    result->numerator = numerator;
    result->denominator = denominator;
    return result;
}

SchemeObject* SchemeObject::createRationalNumber(long numerator, long denominator) {
    i_normalize_rational(&numerator, &denominator);        
    SchemeObject* n = createIntegerNumber(numerator);        
    SchemeObject* d = createIntegerNumber(denominator);
    return createRationalNumber(n, d);
}

SchemeObject* SchemeObject::createRationalNumber(pair<long,long> rational) {
    return createRationalNumber(rational.first, rational.second);   
}

SchemeObject* SchemeObject::createIntegerNumber(long number) {
    SchemeObject* result = Heap::getUniqueInstance()->allocate(SchemeObject::INTEGER_NUMBER);
    result->integer_value = number;
    return result;
}

SchemeObject* SchemeObject::createString(const wchar_t* str) {
    SchemeObject* result = Heap::getUniqueInstance()->allocate(SchemeObject::STRING);
    result->length = wcslen(str);
    result->str = new wchar_t[result->length+1];
    wcscpy(result->str, str);
    return result;
}

SchemeObject* SchemeObject::createChar(wchar_t c) {
    SchemeObject* result = Heap::getUniqueInstance()->allocate(SchemeObject::CHAR);
    result->c = c;
    return result;
}

SchemeObject* SchemeObject::createPair(SchemeObject* car, SchemeObject* cdr) {
    SchemeObject* result = Heap::getUniqueInstance()->allocate(SchemeObject::PAIR);
    result->car = car;
    result->cdr = cdr;
    return result;
}

SchemeObject* SchemeObject::createBool(bool b) {
    SchemeObject* result = Heap::getUniqueInstance()->allocate(SchemeObject::BOOL);
    result->boolean = b;
    return result;
}

SchemeObject* SchemeObject::createVector(SchemeObject* elem, uint32_t length) {
    SchemeObject* result = Heap::getUniqueInstance()->allocate(SchemeObject::VECTOR);
    result->elems = new SchemeObject*[length];
    result->length = length;
    for(uint32_t i = 0; i < length; i++) {
        result->elems[i] = elem;
    }
    return result;
}

SchemeObject* SchemeObject::createEmptyList() {
    SchemeObject* result = Heap::getUniqueInstance()->allocate(SchemeObject::EMPTY_LIST);
    return result;
}

SchemeObject* SchemeObject::createEOF() {
    SchemeObject* result = Heap::getUniqueInstance()->allocate(SchemeObject::EOFTYPE);
    return result;
}

SchemeObject* SchemeObject::createUnspecified() {
    SchemeObject* result = Heap::getUniqueInstance()->allocate(SchemeObject::UNSPECIFIED);
    return result;
}

SchemeObject* SchemeObject::createSymbol(const wchar_t* str) {
    SchemeObject* result;
    wstring strstring = wstring(str);
    map<wstring,SchemeObject*>::iterator v = known_symbols.find(strstring);
    if (v == known_symbols.end()) {
        result = Heap::getUniqueInstance()->allocate(SchemeObject::SYMBOL);
        result->str = new wchar_t[strstring.size()+1];
        wcscpy(result->str, str);
        known_symbols[strstring] = result;
        int h = (int) result;

#if 0
        h += ~(h << 15);
        h ^= (h >> 10);
        h += (h << 3);
        h ^= (h >> 6);
        h += ~(h << 11);
        h ^= (h >> 16);
#else        
        h ^= h << 3;
        h += h >> 5;
        h ^= h << 4;
        h += h >> 17;
        h ^= h << 25;
        h += h >> 6;
#endif        
        result->hash = (h < 0) ? h * -1 : h;
        //result->hash = 128;
    } else {
        result = v->second;
    }
    return result;
}

SchemeObject* SchemeObject::createEnvironment(SchemeObject* parent, uint32_t num_buckets) {
    assert(num_buckets > 0);
    SchemeObject* result;
    if (num_buckets > 8) {
	result = Heap::getUniqueInstance()->allocate(SchemeObject::ENVIRONMENT);
	result->binding_map = new binding_map_t(num_buckets);
    } else {
	result = Heap::getUniqueInstance()->allocate(SchemeObject::SIMPLE_ENVIRONMENT);
	result->binding_list = S_EMPTY_LIST;
    }
    result->parent = parent;
    return result;
}

SchemeObject* SchemeObject::createInputPort(wistream* is) {
    SchemeObject* result = Heap::getUniqueInstance()->allocate(SchemeObject::INPUT_PORT);
    result->is = is;
    return result;
}

SchemeObject* SchemeObject::createOutputPort(wostream* os) {
    SchemeObject* result = Heap::getUniqueInstance()->allocate(SchemeObject::OUTPUT_PORT);
    result->os = os;
    return result;
}

SchemeObject* SchemeObject::createContinuation() {
    SchemeObject* result = Heap::getUniqueInstance()->allocate(SchemeObject::CONTINUATION);
    result->result = NULL;
    return result;
}

SchemeObject* SchemeObject::createBuiltinProcedure(SchemeObject* name, int req, int opt, int rst, SchemeObject* (*fn)()) {
    assert(i_symbol_p(name) == S_TRUE);
    assert(rst == 0 || rst == 1);
    assert(req >= 0 && req < 16);
    assert(opt >= 0 && opt < 16);
    SchemeObject* result = Heap::getUniqueInstance()->allocate(SchemeObject::BUILT_IN_PROCEDURE);
    result->name = name;
    result->metadata |= ((req & 0xf) << REQ_BITS_OFFS);
    result->metadata |= ((opt & 0xf) << OPT_BITS_OFFS);
    if (rst == 1) {
        result->metadata |= REST_FLAG;        
    }
    result->fn = fn;
    return result;
}

SchemeObject* SchemeObject::createUserProcedure(SchemeObject* name, SchemeObject* envt, SchemeObject* s_formals, SchemeObject* s_body) {
    assert(i_symbol_p(name) == S_TRUE);
    ObjectType t = envt->type();
    assert(t == SchemeObject::ENVIRONMENT || t == SchemeObject::SIMPLE_ENVIRONMENT);
    SchemeObject* dup = s_find_duplicate(s_formals);
    if (dup != S_FALSE) {
        throw scheme_exception(L"Duplicate formal " + dup->toString() + L" in " + s_formals->toString());    
    }
    SchemeObject* result = Heap::getUniqueInstance()->allocate(SchemeObject::USER_PROCEDURE);
    result->name = name;
    result->s_closure_data = i_cons(s_formals, i_cons(s_body, envt));
    return result;
}

SchemeObject* SchemeObject::createMacro(SchemeObject* name, SchemeObject* envt, SchemeObject* s_formals, SchemeObject* s_body) {
    assert(i_symbol_p(name) == S_TRUE);
    ObjectType t = envt->type();
    assert(t == SchemeObject::ENVIRONMENT || t == SchemeObject::SIMPLE_ENVIRONMENT);
    SchemeObject* dup = s_find_duplicate(s_formals);
    if (dup != S_FALSE) {
        throw scheme_exception(L"Duplicate formal " + dup->toString() + L" in " + s_formals->toString());    
    }
    SchemeObject* result = Heap::getUniqueInstance()->allocate(SchemeObject::MACRO);
    result->name = name;
    result->s_closure_data = i_cons(s_formals, i_cons(s_body, envt));
    return result;
}

SchemeObject* SchemeObject::createInternalProcedure(const wchar_t* name) {
    SchemeObject* result = Heap::getUniqueInstance()->allocate(SchemeObject::INTERNAL_PROCEDURE);
    result->name = createSymbol(name);
    return result;
}

SchemeObject* SchemeObject::createWrappedCObject(int subtype, SchemeWrappedCObject* object) {
    SchemeObject* result = Heap::getUniqueInstance()->allocate(SchemeObject::WRAPPED_C_OBJECT);
    result->wrapped_object = object;
    result->wrapped_subtype = subtype;
    return result;
}

int SchemeObject::registerWrappedObject() {
    return subtypes_seq++;        
}

void SchemeObject::mark() {
    if (!inuse()) {
        metadata |= INUSE_FLAG;
        ObjectType t = type();
        switch(t) {
            case SchemeObject::PAIR :
                if (car != NULL) car->mark();
                if (cdr != NULL) cdr->mark();
                break;
            case SchemeObject::COMPLEX_NUMBER :
                real->mark();
                imag->mark();
                break;
            case SchemeObject::RATIONAL_NUMBER :
                numerator->mark();
                denominator->mark();
                break;
            case SchemeObject::VECTOR :    
                for(int32_t i = 0; i < length; i++) {
                    elems[i]->mark();
                }
                break;
            case SchemeObject::CONTINUATION: 
                if (result != NULL) {
                    result->mark();
                }
                break;
            case SchemeObject::ENVIRONMENT : {
                binding_map_t::iterator v = binding_map->begin();
                while (v != binding_map->end()) {
                    if ((*v).first != NULL) (*v).first->mark();
                    if ((*v).second != NULL) (*v).second->mark();
                    v++;
                }
                if (parent != NULL) {
                    parent->mark();
                }
                break;
            }
            case SchemeObject::SIMPLE_ENVIRONMENT :
		binding_list->mark();		
                if (parent != NULL) {
                    parent->mark();
                }
                break;		
            case SchemeObject::USER_PROCEDURE :
                s_closure_data->mark();
                if (name != NULL) {
                    name->mark();
                }
                break;
            case SchemeObject::MACRO :
                s_closure_data->mark();
                if (name != NULL) {
                    name->mark();
                }
                break;
            case SchemeObject::BUILT_IN_PROCEDURE :
                if (name != NULL) {
                    name->mark();
                }
                break;
            case SchemeObject::INTERNAL_PROCEDURE :
                if (name != NULL) {
                    name->mark();
                }
                break;
            case SchemeObject::WRAPPED_C_OBJECT :
                wrapped_object->mark();
                break;
            default:
                break;        
        }
    }
}

void SchemeObject::finalize() {
    ObjectType t = type();
    switch(t) {
        case SchemeObject::VECTOR :
            delete [] elems;
            break;
        case SchemeObject::STRING :
            if (!immutable()) {
               //free(str);
            }
            break;
        case SchemeObject::CONTINUATION :
            free(jmpbuf);
            break;
        case SchemeObject::ENVIRONMENT :
            delete binding_map;
            break;
        case SchemeObject::WRAPPED_C_OBJECT :
            wrapped_object->finalize();
            break;
        default:
            break;    
    }    
}

wstring SchemeObject::toString() {
    wostringstream ss;
    ObjectType t = type();
    
    switch(t) {
        case SchemeObject::UNSPECIFIED :
            return L"#<unspecified>";
        case SchemeObject::STRING : {
            wchar_t* s = str;
            ss << L'"';
            while(*s) {
                if (*s == L'\\') {
                    ss << L"\\";        
                } else if (*s == L'"') {
                    ss << L"\\\"";        
                } else {
                    ss << *s;        
                }
                s++;   
            }
            ss << L'"';
            break;
            }
        case SchemeObject::SYMBOL :    
            return wstring(str);
        case SchemeObject::PAIR : {
            if (s_circular_list_p(this) == S_TRUE) {
                return L"#<circular list>";
            }
            ss << L"(";
            SchemeObject *p = this;
            SchemeObject* n;
            while (true) {
        	ss << i_car(p)->toString();
        	n = s_cdr(p);
        	if (n == S_EMPTY_LIST) {
        	    break;
        	}
        	if (s_pair_p(n) == S_FALSE) {
        	    ss << L" . " << n->toString();
        	    break;
        	}
                p = n;
        	ss << L" ";
        	}
        	ss << L")";
    	    }
            break;
        case SchemeObject::COMPLEX_NUMBER:
        case SchemeObject::REAL_NUMBER:	
        case SchemeObject::RATIONAL_NUMBER:
        case SchemeObject::INTEGER_NUMBER:
            return i_number_2_string(this, 10);     
        case SchemeObject::BOOL :    
            return boolean ? L"#t" : L"#f";
        case SchemeObject::VECTOR :    
            ss << L"#(";
            for(int i = 0; i < length; i++) {
                ss << elems[i]->toString();
                if (i < length-1) {
                    ss << " ";
                }
            }
            ss << L")";
            break;
        case SchemeObject::ENVIRONMENT :    
        case SchemeObject::SIMPLE_ENVIRONMENT :    
            return L"#<environment>";
        case SchemeObject::BLANK :
            return L"#<blank heap slot>";
        case SchemeObject::MACRO :
            ss << L"#<macro " << scm2string(name) << L">";
            break;
        case SchemeObject::CONTINUATION: 
            return L"#<continuation>";
        case SchemeObject::USER_PROCEDURE :    
            ss << L"#<primitive-procedure " << scm2string(name) << L">";
            break;
        case SchemeObject::BUILT_IN_PROCEDURE :    
            ss << L"#<built-in-procedure " << scm2string(name) << L">";
            break;
        case SchemeObject::INTERNAL_PROCEDURE :    
            ss << L"#<internal-procedure " << scm2string(name) << L">";
            break;
        case SchemeObject::WRAPPED_C_OBJECT :
            return wrapped_object->toString();
        case SchemeObject::EOFTYPE :
            return L"#<EOF>";
        case SchemeObject::INPUT_PORT :
            return L"#<input-port>";
        case SchemeObject::OUTPUT_PORT :    
            return L"#<output-port>";
        case SchemeObject::CHAR :    
            if (c == L' ') {
                return L"#\\space";
            } else if (c == L'\n') {
                return L"#\\newline";
            } else {
                ss << L"#\\" << wstring(&c,1);
            }
            break;
        case SchemeObject::EMPTY_LIST :
            return L"()";
        default:
            wstringstream ss;
            ss << t;
            throw scheme_exception(L"Unknown type " + ss.str() + L" in toString()");    
    }
    return ss.str();
}

wstring SchemeObject::toString(ObjectType t) {
    switch(t) {
        case SchemeObject::UNSPECIFIED :
            return L"Unspecified";
        case SchemeObject::STRING :
            return L"String";
        case SchemeObject::SYMBOL :    
            return L"Symbol";
        case SchemeObject::PAIR :
            return L"Pair";
        case SchemeObject::COMPLEX_NUMBER:	
            return L"Complex";
        case SchemeObject::REAL_NUMBER:	
            return L"Real";
        case SchemeObject::RATIONAL_NUMBER:	
            return L"Rational";
        case SchemeObject::INTEGER_NUMBER:	
            return L"Integer";
        case SchemeObject::BOOL :    
            return L"Boolean";
        case SchemeObject::VECTOR :    
            return L"Vector";
        case SchemeObject::ENVIRONMENT :    
            return L"Environment";
        case SchemeObject::SIMPLE_ENVIRONMENT :    
            return L"Simple environment";
        case SchemeObject::BLANK :
            return L"Blank heap spot";
        case SchemeObject::MACRO :
            return L"Macro";
        case SchemeObject::CONTINUATION: 
            return L"Continuation";
        case SchemeObject::USER_PROCEDURE :    
            return L"User-procedure";
        case SchemeObject::BUILT_IN_PROCEDURE :    
            return L"Built-in-procedure";
        case SchemeObject::INTERNAL_PROCEDURE :    
            return L"Internal-procedure";
        case SchemeObject::WRAPPED_C_OBJECT :
            return L"Wrapped C-object";
        case SchemeObject::EOFTYPE :
            return L"EOF";
        case SchemeObject::INPUT_PORT :
            return L"Inputport";
        case SchemeObject::OUTPUT_PORT :    
            return L"Outputport";
        case SchemeObject::CHAR :    
            return L"Char";
        case SchemeObject::EMPTY_LIST :
            return L"Empty list";
        default:
            throw scheme_exception(L"Unknown type in toString()");    
    }
}

//-----------------------------------------------------------
// Vector
//-----------------------------------------------------------

SchemeObject* SchemeObject::getVectorElem(int i) {
    assert(type() == SchemeObject::VECTOR);
    return elems[i];
}

void SchemeObject::setVectorElem(SchemeObject* o, int i) {
    assert(type() == SchemeObject::VECTOR);
    elems[i] = o;
}

//-----------------------------------------------------------
// Procedure
//-----------------------------------------------------------

wstring SchemeObject::nameAsString() {
    return wstring(name->str);
}


//-----------------------------------------------------------
// Continuation
//-----------------------------------------------------------

void SchemeObject::callContinuation(SchemeObject* arg) {
    assert(type() == SchemeObject::CONTINUATION);
    this->result = arg;
    longjmp(*jmpbuf, 1);
}

//-----------------------------------------------------------
// Environment
//-----------------------------------------------------------

SchemeObject* SchemeObject::getBinding(SchemeObject* symbol) {
    for (SchemeObject* envt = this; envt != NULL; envt = envt->parent) {
	ObjectType t = envt->type();
        if (t == SchemeObject::SIMPLE_ENVIRONMENT) {
    	    SchemeObject* list = envt->binding_list;
            int i = 0;
	    while (list != S_EMPTY_LIST) {
	        SchemeObject* binding = i_car(list);
	        if (i_car(binding) == symbol) {
                    //cout << "Found at index " << i << endl;        
	 	    return i_cdr(binding);
	        }
	        list = i_cdr(list);
                i++;
	    }
            //cout << "Not found in " << i << endl;
	} else if (t == SchemeObject::ENVIRONMENT) {
            //cout << "Binding map size " << envt->binding_map->size() << endl;        
            binding_map_t::iterator v = envt->binding_map->find(symbol, symbol->hash);
            if (v != envt->binding_map->end()) {
                return v->second;
            }
        } else {
	    throw scheme_exception(L"Not an environment");
	}
    }
    return NULL;
}

void SchemeObject::defineBinding(SchemeObject* symbol, SchemeObject* o) {
    ObjectType t = type();
    if (t == SchemeObject::SIMPLE_ENVIRONMENT) {
    	SchemeObject* binding = i_cons(symbol,o);
    	// Prepend the new binding pair
    	binding_list = i_cons(binding, binding_list);
    } else if (t == SchemeObject::ENVIRONMENT) {
	// Insert into map
	binding_map->insert(binding_map_t::value_type(symbol,o), symbol->hash);
    } else {
	throw scheme_exception(L"Not an environment");
    }
}

void SchemeObject::setBinding(SchemeObject* symbol, SchemeObject* o) {
    for(SchemeObject* envt = this; envt != NULL; envt = envt->parent) {
	ObjectType t = envt->type();
	if (t == SchemeObject::SIMPLE_ENVIRONMENT) {
	    SchemeObject* list = envt->binding_list;
	    while (list != S_EMPTY_LIST) {
		SchemeObject* binding = i_car(list);
		if (i_car(binding) == symbol) {
		    i_set_cdr_e(binding, o);
		    return;
		}
		list = i_cdr(list);
	    }
	} else if (t == SchemeObject::ENVIRONMENT) {
            binding_map_t::iterator v = envt->binding_map->find(symbol, symbol->hash);
            if (v != envt->binding_map->end()) {
                v->second = o;  
                return;
       	    }
    	} else {
	    throw scheme_exception(L"Not an environment");
	}
    }
    throw scheme_exception(L"Unbound variable: " + symbol->toString());
}

//-----------------------------------------------------------
// Wrapped objects
//-----------------------------------------------------------
SchemeWrappedCObject::~SchemeWrappedCObject() {
}

wstring SchemeWrappedCObject::toString() {
    return L"#<wrapped-c-object>";
}

/**
 * If the wrapped object contains pointers to other scheme objects
 * this method should be overridden to call the their mark() method.
 */
void SchemeWrappedCObject::mark() {
}

void SchemeWrappedCObject::finalize() {
}

