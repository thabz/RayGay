
#include "objects.h"
#include "scheme.h"
#include <sstream>
#include "heap.h"

// Map of known symbols
map<char*,SchemeObject*> SchemeObject::known_symbols;

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
    char* copy = strdup(str);
    map<string,SchemeObject*>::iterator v = known_symbols.find(string(str));
    if (v == known_symbols.end()) {
        result = Heap::getUniqueInstance()->allocate(SchemeObject::SYMBOL);
        result->str = strdup(str);
        known_symbols[stringstr] = result;
    } else {
        result = v->second;
    }
    return result;
}

SchemeObject* SchemeObject::createEnvironment(SchemeObject* parent) {
    SchemeObject* result = Heap::getUniqueInstance()->allocate(SchemeObject::ENVIRONMENT);
    result->binding_map = new map<SchemeObject*,SchemeObject*>();
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
    SchemeObject* result = Heap::getUniqueInstance()->allocate(SchemeObject::OUTPUT_PORT);
    result->result = NULL;
    return result;
}

SchemeObject* SchemeObject::createBuiltinProcedure(SchemeObject* name, int req, int opt, int rst, SchemeObject* (*fn)()) {
    assert(i_symbol_p(name) == S_TRUE);
    assert(rst == 0 || rst == 1);
    SchemeObject* result = Heap::getUniqueInstance()->allocate(SchemeObject::BUILT_IN_PROCEDURE);
    result->name = name;
    result->req = req;
    result->opt = opt;
    result->rst = rst;
    result->fn = fn;
    return result;
}

SchemeObject* SchemeObject::createUserProcedure(SchemeObject* name, SchemeObject* envt, SchemeObject* s_formals, SchemeObject* s_body) {
    assert(i_symbol_p(name) == S_TRUE);
    SchemeObject* result = Heap::getUniqueInstance()->allocate(SchemeObject::USER_PROCEDURE);
    result->name = name;
    result->envt = envt;
    result->s_formals = s_formals;
    result->s_body = s_body;
    return result;
}

SchemeObject* SchemeObject::createMacro(SchemeObject* name, SchemeObject* envt, SchemeObject* s_formals, SchemeObject* s_body) {
    assert(i_symbol_p(name) == S_TRUE);
    SchemeObject* result = Heap::getUniqueInstance()->allocate(SchemeObject::MACRO);
    result->name = name;
    result->envt = envt;
    result->s_formals = s_formals;
    result->s_body = s_body;
    return result;
}

SchemeObject* SchemeObject::createInternalProcedure(SchemeObject* name) {
    assert(i_symbol_p(name) == S_TRUE);
    SchemeObject* result = Heap::getUniqueInstance()->allocate(SchemeObject::INTERNAL_PROCEDURE);
    result->name = name;
    return result;
}

void SchemeObject::mark() {
    map<SchemeObject*,SchemeObject*>::iterator v;
    if (type_and_flags && 0xff000000 == 0) {
        type_and_flags |= 0xff000000;
        ObjectType t = type();
        switch(t) {
            case SchemeObject::PAIR :
                if (car != NULL) car->mark();
                if (cdr != NULL) cdr->mark();
                break;
            case SchemeObject::VECTOR :    
                for(uint32_t i = 0; i < length; i++) {
                    elems[i]->mark();
                }
                break;
            case SchemeObject::CONTINUATION: 
                if (result != NULL) {
                    result->mark();
                }
                break;
            case SchemeObject::ENVIRONMENT :    
                v = binding_map->begin();
                while (v != binding_map->end()) {
                    if ((*v).first != NULL) (*v).first->mark();
                    if ((*v).second != NULL) (*v).second->mark();
                    v++;
                }
                if (parent != NULL) {
                    parent->mark();
                }
                break;
            case SchemeObject::USER_PROCEDURE :
                envt->mark();
                s_formals->mark();
                s_body->mark(); 
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
            default:
                break;        
        }
    }
}

SchemeObject::~SchemeObject() {
    ObjectType t = type();
    switch(t) {
        case SchemeObject::VECTOR :
            delete [] elems;
            break;
    }    
}

void SchemeObject::clear_inuse() {
    type_and_flags = type_and_flags & 0x00ffffff;
}

bool SchemeObject::immutable() const {
    return type_and_flags && 0x00ff0000 == 0;
}

void SchemeObject::set_immutable(bool flag) {
    if (flag) {
        type_and_flags |= 0x00ff0000;
    } else {
        type_and_flags &= 0xff00ffff;
        
    }
}

string SchemeObject::toString() {
    ostringstream ss;
    ObjectType t = type();
    switch(t) {
        case SchemeObject::UNSPECIFIED :
            return "#<unspecified>";
        case SchemeObject::STRING :    
            // TODO: s!"!\"!g and s!\!\\!g
            return "\"" + string(str) + "\"";
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
        case SchemeObject::CONTINUATION: 
            return "#<continuation>";
        case SchemeObject::USER_PROCEDURE :    
            ss << "#<primitive-procedure " << scm2string(name) << ">";
        case SchemeObject::BUILT_IN_PROCEDURE :    
            ss << "#<built-in-procedure " << scm2string(name) << ">";
        case SchemeObject::INTERNAL_PROCEDURE :    
            ss << "#<internal-procedure " << scm2string(name) << ">";
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
        case SchemeObject::EMPTY_LIST :
            return "()";
    }
	return ss.str();
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
    longjmp(jmpbuf, 1);
}

//-----------------------------------------------------------
// Environment
//-----------------------------------------------------------

SchemeObject* SchemeObject::getBinding(SchemeObject* name) {
    assert(type() == SchemeObject::ENVIRONMENT);
    if (s_symbol_p(name) == S_FALSE) {
        throw scheme_exception(name->toString() + " is not a symbol.");
    }
    map<SchemeObject*,SchemeObject*>::iterator v = binding_map->find(name);
    if (v == binding_map->end()) {
        if (parent != NULL) {
            return parent->getBinding(name);
        } else {
            return NULL;
        }
    } else {
        return v->second;
    }
}

void SchemeObject::putBinding(SchemeObject* name, SchemeObject* o) {
    assert(type() == SchemeObject::ENVIRONMENT);
    if (i_symbol_p(name) == S_FALSE) {
        throw scheme_exception(name->toString() + " is not a symbol.");
    }
    (*binding_map)[name] = o;
}

void SchemeObject::setBinding(SchemeObject* name, SchemeObject* o) {
    assert(type() == SchemeObject::ENVIRONMENT);
    if (i_symbol_p(name) == S_FALSE) {
        throw scheme_exception(name->toString() + " is not a symbol.");
    }
    map<SchemeObject*,SchemeObject*>::iterator v = binding_map->find(name);
    if (v == binding_map->end()) {
        if (parent != NULL) {
            parent->setBinding(name,o);
        } else {
            throw scheme_exception("Unbound variable: " + name->toString());
        }
    } else {
        putBinding(name,o);
    }
}


