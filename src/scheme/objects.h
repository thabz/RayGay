
#ifndef SCHEME_OBJECTS_H
#define SCHEME_OBJECTS_H

#include <string>
#include <map>
#include <setjmp.h>

using namespace std;

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
#define i_number_p(o)    ((o)->type() == SchemeObject::NUMBER ? S_TRUE : S_FALSE)
#define i_procedure_p(p) (((p)->type() == SchemeObject::BUILT_IN_PROCEDURE ||  \
                           (p)->type() == SchemeObject::CONTINUATION       ||  \
                           (p)->type() == SchemeObject::USER_PROCEDURE     ||  \
                           (p)->type() == SchemeObject::INTERNAL_PROCEDURE) ? S_TRUE : S_FALSE)
#define i_null_p(o)      ((o) == S_EMPTY_LIST ? S_TRUE : S_FALSE)
#define i_cons(car,cdr)  (SchemeObject::createPair((car),(cdr)))


#define IMMUTABLE_FLAG ((uint32_t)(1 << 31))
#define INUSE_FLAG     ((uint32_t)(1 << 30))
#define REST_FLAG      ((uint32_t)(1 << 29))
#define REQ_BITS_OFFS  16
#define OPT_BITS_OFFS  20

class SchemeObject 
{
    public:
        uint32_t metadata;
        union {
            double value;                  // For numbers
            struct {
                union {
                    char* str;             // For strings and symbols
                    SchemeObject* car;     // For pairs
                    bool boolean;          // For booleans
                    ::jmp_buf *jmpbuf;     // For continuations
                    istream* is;           // For inputports
                    ostream* os;           // For outputports
                    char c;                // For chars
                    SchemeObject** elems;  // For vector
                    SchemeObject* parent;  // For environments. Environment.
                    SchemeObject* name;    // For macros and procedures. Symbol.
                };
                union {
                    SchemeObject* cdr;      // For pairs
                    SchemeObject* result;   // For continuations
                    int32_t length;         // For vector and strings
                    map<SchemeObject*,SchemeObject*>* binding_map;	// For environments
                    SchemeObject* (*fn)();  // For BUILT_IN_PROCEDURE
                    SchemeObject* s_closure_data;   // For USER_PROCEDURE (formals body . envt)
                };
            };
        };

    public:        
        enum ObjectType {
 		    BLANK,                  // Empty slots in heap
		    NUMBER,
		    EMPTY_LIST,
		    BOOL,
		    CHAR,
		    STRING,
		    VECTOR,
 		    EOFTYPE,
 		    INPUT_PORT,
 		    OUTPUT_PORT,
		    SELF_EVALUATING_FORMS_ARE_BEFORE_HERE,
		    SYMBOL,
		    PAIR,
		    UNSPECIFIED,
 		    USER_PROCEDURE,
 		    INTERNAL_PROCEDURE,
 		    BUILT_IN_PROCEDURE,
 		    MACRO,
 		    CONTINUATION,
 		    ENVIRONMENT
		};

    public:
        ObjectType type() const;
        bool immutable() const;
        void set_immutable(bool flag);
        string toString();
        void clear_inuse();
        bool inuse() const;
        void mark();
        void finalize();
        bool self_evaluating() const;

        SchemeObject* getVectorElem(int index);
        void setVectorElem(SchemeObject* o, int index);

	    SchemeObject* getBinding(SchemeObject* name);
        void defineBinding(SchemeObject* name, SchemeObject* o);
        void setBinding(SchemeObject* name, SchemeObject* o);
        
        string nameAsString();

        // For USER_PROCEDURE and MACRO
        SchemeObject* s_formals();
        SchemeObject* s_body();
        SchemeObject* s_envt();
        
        void callContinuation(SchemeObject* arg);
        
        // For BUILT_IN_PROCEDURE.
        bool rest() const;    // Takes rest argument?
        uint32_t req() const; // No. of required arguments
        uint32_t opt() const; // No. of optional arguments
        
        static SchemeObject* createNumber(double number);
        static SchemeObject* createString(const char* str);
        static SchemeObject* createChar(char c);
        static SchemeObject* createPair(SchemeObject* car, SchemeObject* cdr);
        static SchemeObject* createVector(SchemeObject* elem, uint32_t length);
        static SchemeObject* createBool(bool b);
        static SchemeObject* createEmptyList();
        static SchemeObject* createUnspecified();
        static SchemeObject* createEOF();
        static SchemeObject* createSymbol(const char* str);
        static SchemeObject* createContinuation();
        static SchemeObject* createEnvironment(SchemeObject* parent);
        static SchemeObject* createInputPort(istream* is);
        static SchemeObject* createOutputPort(ostream* os);
        static SchemeObject* createBuiltinProcedure(SchemeObject* name, int req, int opt, int rst, SchemeObject* (*fn)());
        static SchemeObject* createUserProcedure(SchemeObject* name, SchemeObject* envt, SchemeObject* s_formals, SchemeObject* s_body);
        static SchemeObject* createInternalProcedure(const char* name);
        static SchemeObject* createMacro(SchemeObject* name, SchemeObject* envt, SchemeObject* s_formals, SchemeObject* s_body);

    private:
        static map<string,SchemeObject*> known_symbols;
        
};

inline
SchemeObject::ObjectType SchemeObject::type() const {
    return ObjectType(metadata & 0x0000ffff);
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

#endif