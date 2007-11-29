
#ifndef SCHEME_OBJECTS_H
#define SCHEME_OBJECTS_H

#include <string>
#include <map>
#include <csetjmp>
#include <cassert>
#include <complex>

#include "collections/bucket_map.h"
#include "bigint.h"
#include "rational.h"

using namespace std;

typedef rational<int64_t> rational_type;

// Faster internal macro for some much used procedures
// that does no argument checking.
#define i_car(o)         ((o)->car)
#define i_cdr(o)         ((o)->cdr)
#define i_caar(o)        (((o)->car)->car)
#define i_cadr(o)        (((o)->cdr)->car)
#define i_cdar(o)        (((o)->car)->cdr)
#define i_cddr(o)        (((o)->cdr)->cdr)
#define i_set_cdr_e(o,v) ((o)->cdr = (v))
#define i_set_car_e(o,v) ((o)->car = (v))
#define i_pair_p(o)      ((o)->type() == SchemeObject::PAIR ? S_TRUE : S_FALSE)
#define i_char_p(o)      ((o)->type() == SchemeObject::CHAR ? S_TRUE : S_FALSE)
#define i_symbol_p(o)    ((o)->type() == SchemeObject::SYMBOL ? S_TRUE : S_FALSE)
#define i_vector_p(o)    ((o)->type() == SchemeObject::VECTOR ? S_TRUE : S_FALSE)
#define i_number_p(o)    ((o)->type() < SchemeObject::NUMBERS_ARE_BEFORE_HERE ? S_TRUE : S_FALSE)
#define i_procedure_p(p) (((p)->type() == SchemeObject::BUILT_IN_PROCEDURE ||  \
                           (p)->type() == SchemeObject::CONTINUATION       ||  \
                           (p)->type() == SchemeObject::USER_PROCEDURE     ||  \
                           (p)->type() == SchemeObject::INTERNAL_PROCEDURE) ? S_TRUE : S_FALSE)
#define i_null_p(o)      ((o) == S_EMPTY_LIST ? S_TRUE : S_FALSE)
#define i_cons(car,cdr)  (SchemeObject::createPair((car),(cdr)))

#define i_list_1(a)      (i_cons((a), S_EMPTY_LIST))
#define i_list_2(a,b)    (i_cons((a), i_cons((b), S_EMPTY_LIST)))
#define i_list_3(a,b,c)  (i_cons((a), i_cons((b), i_cons((c), S_EMPTY_LIST))))

#define i_vector_ref(v,i) ((v)->elems[i])
#define i_vector_length(v) ((v)->length)
#define i_vector_set_e(v,i,e) ((v)->elems[i] = (e))
#define i_make_vector(c,e) (SchemeObject::createVector((e), (c)))
#define i_wrapped_object_p(o,subtype) (((o)->type() == SchemeObject::WRAPPED_C_OBJECT && (o)->wrapped_subtype == (subtype)) ? S_TRUE : S_FALSE)
#define IMMUTABLE_FLAG ((uint32_t)(1 << 31))
#define INUSE_FLAG     ((uint32_t)(1 << 30))
#define REST_FLAG      ((uint32_t)(1 << 29))
#define GC_PROTECTED   ((uint32_t)(1 << 28))
#define REQ_BITS_OFFS  8
#define OPT_BITS_OFFS  12
#define SRC_LINE_OFFS  8

class SchemeWrappedCObject {
    public:
        virtual ~SchemeWrappedCObject();    
        virtual wstring toString();
        virtual void mark();
        virtual void finalize();
};

class SchemeObject 
{
    private:
        typedef bucket_map<SchemeObject*,SchemeObject*> binding_map_t;
        
    public:
        uint32_t metadata;
        union {
            double real_value;             // For real numbers
            int64_t integer_value;          // For integer numbers
            struct {
                union {
                    wchar_t* str;          // For strings and symbols
                    SchemeObject* car;     // For pairs
                    bool boolean;          // For booleans
                    ::jmp_buf *jmpbuf;     // For continuations
                    wistream* is;          // For inputports
                    wostream* os;          // For outputports
                    wchar_t c;             // For chars
                    SchemeObject** elems;  // For vector
                    SchemeObject* parent;  // For environments. Environment.
                    SchemeObject* name;    // For macros and procedures. Symbol.
                    SchemeObject* real;    // For complex numbers
                    SchemeObject* numerator;// For rational numbers
                    int32_t wrapped_subtype;// For wrapped C-objects
                };
                union {
                    SchemeObject* cdr;      // For pairs
                    SchemeObject* result;   // For continuations
                    int32_t length;         // For vector and strings
                    binding_map_t* binding_map;	// For environments
		    SchemeObject* binding_list; // For simple environments 
                    SchemeObject* (*fn)();  // For BUILT_IN_PROCEDURE
                    SchemeObject* s_closure_data;   // For USER_PROCEDURE (formals body . envt)
                    SchemeObject* imag;    // For complex numbers
                    SchemeObject* denominator;// For rational numbers
                    SchemeWrappedCObject* wrapped_object; // For wrapped C-objects
                    binding_map_t::hash_type hash;  // For symbols 
                };
            };
        };

    public:        
        enum ObjectType {
		    COMPLEX_NUMBER,
		    REAL_NUMBER,
		    RATIONAL_NUMBER,
		    INTEGER_NUMBER,
		    NUMBERS_ARE_BEFORE_HERE,
 		    BLANK,                      // Empty slots in heap
		    RESERVED,		            // Thead-reserved slots in heap
		    EMPTY_LIST,
		    BOOL,
		    CHAR,
		    STRING,
		    VECTOR,
 		    EOFTYPE,
 		    INPUT_PORT,
 		    OUTPUT_PORT,
 		    WRAPPED_C_OBJECT,
		    UNSPECIFIED,
		    SELF_EVALUATING_FORMS_ARE_BEFORE_HERE,
		    SYMBOL,
		    PAIR,
 		    USER_PROCEDURE,
 		    INTERNAL_PROCEDURE,
 		    BUILT_IN_PROCEDURE,
 		    MACRO,
 		    CONTINUATION,
 		    ENVIRONMENT,
		    SIMPLE_ENVIRONMENT,
 		    ALL_TYPE_ARE_BEFORE_HERE
		};

    public:
        ObjectType type() const;
        bool immutable() const;
        void set_immutable(bool flag);
        bool gc_protected() const;
        void set_gc_protected(bool flag);
        wstring toString();
        void clear_inuse();
        bool inuse() const;
        void mark();
        void finalize();
        bool self_evaluating() const;
        uint32_t src_line() const;
        void set_src_line(uint32_t line);
        
        wstring wstr();

        SchemeObject* getVectorElem(int index);
        void setVectorElem(SchemeObject* o, int index);

	    SchemeObject* getBinding(SchemeObject* name);
        void defineBinding(SchemeObject* name, SchemeObject* o);
        void setBinding(SchemeObject* name, SchemeObject* o);
        vector<SchemeObject*> getBindingKeys();
        wstring nameAsString();
        
        // For WRAPPED_C_OBJECT
        static int registerWrappedObject();
        void* getWrappedCObject();

        // For USER_PROCEDURE and MACRO
        SchemeObject* s_formals();
        SchemeObject* s_body();
        SchemeObject* s_envt();
        
        void callContinuation(SchemeObject* arg);
        
        // For numbers
        std::complex<double> complexValue() const;
        double realValue() const;
        rational_type rationalValue() const;
        int64_t integerValue() const;
        
        // For BUILT_IN_PROCEDURE.
        bool rest() const;    // Takes rest argument?
        uint32_t req() const; // No. of required arguments
        uint32_t opt() const; // No. of optional arguments
        
        static SchemeObject* createComplexNumber(std::complex<double> c);
        static SchemeObject* createComplexNumber(SchemeObject* real, SchemeObject* imag);
        static SchemeObject* createRealNumber(double number);
        static SchemeObject* createRationalNumber(SchemeObject* numerator, SchemeObject* denominator);
        static SchemeObject* createRationalNumber(rational_type::value_type n, rational_type::value_type d);
        static SchemeObject* createRationalNumber(rational_type rational);
        static SchemeObject* createIntegerNumber(int64_t number);
        static SchemeObject* createString(const wchar_t* str);
        static SchemeObject* createChar(wchar_t c);
        static SchemeObject* createPair(SchemeObject* car, SchemeObject* cdr);
        static SchemeObject* createVector(SchemeObject* elem, uint32_t length);
        static SchemeObject* createBool(bool b);
        static SchemeObject* createEmptyList();
        static SchemeObject* createUnspecified();
        static SchemeObject* createEOF();
        static SchemeObject* createSymbol(const wchar_t* str);
        static SchemeObject* createContinuation();
        static SchemeObject* createEnvironment(SchemeObject* parent, uint32_t num_buckets = 8);
        static SchemeObject* createInputPort(wistream* is);
        static SchemeObject* createOutputPort(wostream* os);
        static SchemeObject* createBuiltinProcedure(SchemeObject* name, int req, int opt, int rst, SchemeObject* (*fn)());
        static SchemeObject* createUserProcedure(SchemeObject* name, SchemeObject* envt, SchemeObject* s_formals, SchemeObject* s_body);
        static SchemeObject* createInternalProcedure(const wchar_t* name);
        static SchemeObject* createMacro(SchemeObject* name, SchemeObject* envt, SchemeObject* s_formals, SchemeObject* s_body);
        static SchemeObject* createWrappedCObject(int subtype, SchemeWrappedCObject*);
        
        // For stats
        static wstring toString(ObjectType type);

    private:
        static map<wstring,SchemeObject*> known_symbols;
        static int subtypes_seq;
        
};

inline
SchemeObject::ObjectType SchemeObject::type() const {
    return ObjectType(metadata & 0x000000ff);
}

inline
bool SchemeObject::rest() const {
    return (metadata & REST_FLAG) != 0;    
}

inline
uint32_t SchemeObject::req() const {
    return (metadata >> REQ_BITS_OFFS) & 0xf;    
}

inline
uint32_t SchemeObject::opt() const {
    return (metadata >> OPT_BITS_OFFS) & 0xf;    
}

inline
uint32_t SchemeObject::src_line() const {
    assert(type() == PAIR || type() == SYMBOL);
    return (metadata >> SRC_LINE_OFFS) & 0x0fff;
}

inline
void SchemeObject::set_src_line(uint32_t line) {
    assert(type() == PAIR || type() == SYMBOL);
    assert(line < 1 << 24);
    metadata |= (line << SRC_LINE_OFFS);
}


inline
void SchemeObject::clear_inuse() {
    metadata &= ~INUSE_FLAG;
}

inline
bool SchemeObject::inuse() const {
    return (metadata & INUSE_FLAG) != 0;
}

inline
void SchemeObject::set_immutable(bool flag) {
    if (flag) {
        metadata |= IMMUTABLE_FLAG;
    } else {
        metadata &= ~IMMUTABLE_FLAG;
    }
}

inline
bool SchemeObject::immutable() const {
    return (metadata & IMMUTABLE_FLAG) != 0;
}

inline
void SchemeObject::set_gc_protected(bool flag) {
    if (flag) {
        metadata |= GC_PROTECTED;
    } else {
        metadata &= ~GC_PROTECTED;
    }
}

inline
bool SchemeObject::gc_protected() const {
    return (metadata & GC_PROTECTED) != 0;
}

inline
SchemeObject* SchemeObject::s_formals() {
    return i_car(s_closure_data);
}

inline
SchemeObject* SchemeObject::s_body() {
    return i_cadr(s_closure_data);
};

inline
SchemeObject* SchemeObject::s_envt() {
    return i_cddr(s_closure_data);
}

inline
bool SchemeObject::self_evaluating() const {
    return type() < SchemeObject::SELF_EVALUATING_FORMS_ARE_BEFORE_HERE;
}

inline
void* SchemeObject::getWrappedCObject() { 
        return wrapped_object; 
}

inline
int64_t SchemeObject::integerValue() const {
    ObjectType t = type();
    if (t == INTEGER_NUMBER) return integer_value;
    else if (t == REAL_NUMBER) return int64_t(real_value);
    else if (t == RATIONAL_NUMBER) return numerator->integer_value;
    else if (t == COMPLEX_NUMBER) return int64_t(real->real_value);
    else return -1; // Shouldn't happen
}

inline
rational_type SchemeObject::rationalValue() const {
    ObjectType t = type();
    if (t == RATIONAL_NUMBER) {
        return rational_type(numerator->integer_value, denominator->integer_value);
    } else { 
        return rational_type(integerValue());
    }
}

inline
double SchemeObject::realValue() const {
    ObjectType t = type();
    if (t == REAL_NUMBER) return real_value;
    else if (t == INTEGER_NUMBER) return double(integer_value);
    else if (t == RATIONAL_NUMBER) return double(numerator->integer_value) / double(denominator->integer_value);
    else if (t == COMPLEX_NUMBER) return real->real_value;
    else return -1; // Shouldn't happen
}

inline
std::complex<double> SchemeObject::complexValue() const {
    ObjectType t = type();
    if (t == COMPLEX_NUMBER) {
        return std::complex<double>(car->realValue(), cdr->realValue());    
    } else {
        return std::complex<double>(realValue(), 0);    
    }
}

#endif
