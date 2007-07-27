
#include "objects.h"
#include "scheme.h"
#include <sstream>
#include "heap.h"

// Map of known symbols
map<string,SchemeObject*> SchemeObject::known_symbols;

// Sequence for subtype identities
int SchemeObject::subtypes_seq = 1;

//-----------------------------------------------------------
// Static factory methods
//-----------------------------------------------------------

SchemeObject* SchemeObject::createNumber(double number) {
    SchemeObject* result = Heap::getUniqueInstance()->allocate(SchemeObject::NUMBER);
    result->value = number;
    return result;
}

SchemeObject* SchemeObject::createString(const char* str) {
    SchemeObject* result = Heap::getUniqueInstance()->allocate(SchemeObject::STRING);
    result->str = strdup(str);
    result->length = strlen(str);
    return result;
}

SchemeObject* SchemeObject::createChar(char c) {
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

SchemeObject* SchemeObject::createSymbol(const char* str) {
    SchemeObject* result;
    string strstring = string(str);
    map<string,SchemeObject*>::iterator v = known_symbols.find(strstring);
    if (v == known_symbols.end()) {
        result = Heap::getUniqueInstance()->allocate(SchemeObject::SYMBOL);
        result->str = strdup(str);
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
    SchemeObject* result = Heap::getUniqueInstance()->allocate(SchemeObject::ENVIRONMENT);
    result->binding_map = new binding_map_t(num_buckets);
    result->parent = parent;
    return result;
}

SchemeObject* SchemeObject::createInputPort(istream* is) {
    SchemeObject* result = Heap::getUniqueInstance()->allocate(SchemeObject::INPUT_PORT);
    result->is = is;
    return result;
}

SchemeObject* SchemeObject::createOutputPort(ostream* os) {
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
    assert(envt->type() == SchemeObject::ENVIRONMENT);
    SchemeObject* dup = s_find_duplicate(s_formals);
    if (dup != S_FALSE) {
        throw scheme_exception("Duplicate formal " + dup->toString() + " in " + s_formals->toString());    
    }
    SchemeObject* result = Heap::getUniqueInstance()->allocate(SchemeObject::USER_PROCEDURE);
    result->name = name;
    result->s_closure_data = i_cons(s_formals, i_cons(s_body, envt));
    return result;
}

SchemeObject* SchemeObject::createMacro(SchemeObject* name, SchemeObject* envt, SchemeObject* s_formals, SchemeObject* s_body) {
    assert(i_symbol_p(name) == S_TRUE);
    assert(envt->type() == SchemeObject::ENVIRONMENT);
    SchemeObject* dup = s_find_duplicate(s_formals);
    if (dup != S_FALSE) {
        throw scheme_exception("Duplicate formal " + dup->toString() + " in " + s_formals->toString());    
    }
    SchemeObject* result = Heap::getUniqueInstance()->allocate(SchemeObject::MACRO);
    result->name = name;
    result->s_closure_data = i_cons(s_formals, i_cons(s_body, envt));
    return result;
}

SchemeObject* SchemeObject::createInternalProcedure(const char* name) {
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

string SchemeObject::toString() {
    ostringstream ss;
    ObjectType t = type();
    switch(t) {
        case SchemeObject::UNSPECIFIED :
            return "#<unspecified>";
        case SchemeObject::STRING : {
            char* s = str;
            ss << '"';
            while(*s) {
                if (*s == '\\') {
                    ss << "\\";        
                } else if (*s == '"') {
                    ss << "\\\"";        
                } else {
                    ss << *s;        
                }
                s++;   
            }
            ss << '"';
            break;
            }
        case SchemeObject::SYMBOL :    
            return string(str);
        case SchemeObject::PAIR : {
            if (s_circular_list_p(this) == S_TRUE) {
                return "#<circular list>";
            }
        	ss << "(";
            SchemeObject *p = this;
            SchemeObject* n;
            while (true) {
        		ss << i_car(p)->toString();
        		n = s_cdr(p);
        		if (n == S_EMPTY_LIST) {
        			break;
        		}
        		if (s_pair_p(n) == S_FALSE) {
        			ss << " . " << n->toString();
        			break;
        		}
                p = n;
        		ss << " ";
        	}
        	ss << ")";
    	    }
            break;
        case SchemeObject::NUMBER:	
            ss << value;
            break;
        case SchemeObject::BOOL :    
            return boolean ? "#t" : "#f";
        case SchemeObject::VECTOR :    
            ss << "#(";
            for(int i = 0; i < length; i++) {
                ss << elems[i]->toString();
                if (i < length-1) {
                    ss << " ";
                }
            }
            ss << ")";
            break;
        case SchemeObject::ENVIRONMENT :    
            return "#<environment>";
        case SchemeObject::BLANK :
            ss << "#<blank heap slot>";
            break;
        case SchemeObject::MACRO :
            ss << "#<macro " << scm2string(name) << ">";
            break;
        case SchemeObject::CONTINUATION: 
            return "#<continuation>";
        case SchemeObject::USER_PROCEDURE :    
            ss << "#<primitive-procedure " << scm2string(name) << ">";
            break;
        case SchemeObject::BUILT_IN_PROCEDURE :    
            ss << "#<built-in-procedure " << scm2string(name) << ">";
            break;
        case SchemeObject::INTERNAL_PROCEDURE :    
            ss << "#<internal-procedure " << scm2string(name) << ">";
            break;
        case SchemeObject::WRAPPED_C_OBJECT :
            return wrapped_object->toString();
        case SchemeObject::EOFTYPE :
            return "#<EOF>";
        case SchemeObject::INPUT_PORT :
            return "#<input-port>";
        case SchemeObject::OUTPUT_PORT :    
            return "#<output-port>";
        case SchemeObject::CHAR :    
            if (c == ' ') {
                return string("#\\space");
            } else if (c == '\n') {
                return string("#\\newline");
            } else {
                ss << "#\\" << string(&c,1);
            }
            break;
        case SchemeObject::EMPTY_LIST :
            return "()";
        default:
            stringstream ss;
            ss << t;
            throw scheme_exception("Unknown type " + ss.str() + " in toString()");    
    }
    return ss.str();
}

string SchemeObject::toString(ObjectType t) {
    switch(t) {
        case SchemeObject::UNSPECIFIED :
            return "Unspecified";
        case SchemeObject::STRING :
            return "String";
        case SchemeObject::SYMBOL :    
            return "Symbol";
        case SchemeObject::PAIR :
            return "Pair";
        case SchemeObject::NUMBER:	
            return "Number";
        case SchemeObject::BOOL :    
            return "Boolean";
        case SchemeObject::VECTOR :    
            return "Vector";
        case SchemeObject::ENVIRONMENT :    
            return "Environment";
        case SchemeObject::BLANK :
            return "Blank heap spot";
        case SchemeObject::MACRO :
            return "Macro";
        case SchemeObject::CONTINUATION: 
            return "Continuation";
        case SchemeObject::USER_PROCEDURE :    
            return "User-procedure";
        case SchemeObject::BUILT_IN_PROCEDURE :    
            return "Built-in-procedure";
        case SchemeObject::INTERNAL_PROCEDURE :    
            return "Internal-procedure";
        case SchemeObject::WRAPPED_C_OBJECT :
            return "Wrapped C-object";
        case SchemeObject::EOFTYPE :
            return "EOF";
        case SchemeObject::INPUT_PORT :
            return "Inputport";
        case SchemeObject::OUTPUT_PORT :    
            return "Outputport";
        case SchemeObject::CHAR :    
            return "Char";
        case SchemeObject::EMPTY_LIST :
            return "Empty list";
        default:
            throw scheme_exception("Unknown type in toString()");    
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

string SchemeObject::nameAsString() {
    return string(name->str);
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

SchemeObject* SchemeObject::getBinding(SchemeObject* name) {
    assert(type() == SchemeObject::ENVIRONMENT);
    assert(name->type() == SchemeObject::SYMBOL);

    for(SchemeObject* envt = this; envt != NULL; envt = envt->parent) {
        binding_map_t::iterator v = envt->binding_map->find(name, name->hash);
        if (v != envt->binding_map->end()) {
            return v->second;
        }
    }
    return NULL;
}

void SchemeObject::defineBinding(SchemeObject* name, SchemeObject* o) {
    assert(type() == SchemeObject::ENVIRONMENT);
    assert(name->type() == SchemeObject::SYMBOL);

    binding_map->insert(binding_map_t::value_type(name,o), name->hash);
}

void SchemeObject::setBinding(SchemeObject* name, SchemeObject* o) {
    assert(type() == SchemeObject::ENVIRONMENT);
    assert(name->type() == SchemeObject::SYMBOL);

    for(SchemeObject* envt = this; envt != NULL; envt = envt->parent) {
        binding_map_t::iterator v = envt->binding_map->find(name, name->hash);
        if (v != envt->binding_map->end()) {
            v->second = o;  
            return;
        }
    }
    throw scheme_exception("Unbound variable: " + name->toString());
}

//-----------------------------------------------------------
// Wrapped objects
//-----------------------------------------------------------
SchemeWrappedCObject::~SchemeWrappedCObject() {
}

string SchemeWrappedCObject::toString() {
    return "#<wrapped-c-object>";
}

/**
 * If the wrapped object contains pointers to other scheme objects
 * this method should be overridden to call the their mark() method.
 */
void SchemeWrappedCObject::mark() {
}

void SchemeWrappedCObject::finalize() {
}

