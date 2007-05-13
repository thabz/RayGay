
#ifndef SCHEME_OBJECTS_H
#define SCHEME_OBJECTS_H

#include <string>
#include <map>
#include <setjmp.h>

class SchemeEnvironment;

using namespace std;

class SchemeObject 
{
    public:
        uint32_t type_and_flags;
        union {
            char* str;             // For strings and symbols
            SchemeObject* car;     // For pairs
            double value;          // For numbers
            bool boolean;          // For booleans
            ::jmp_buf jmpbuf;      // For continuations
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
            uint32_t length;        // For vector
            map<SchemeObject*,SchemeObject*>* binding_map;	// For environments
            int req;                // For BUILT_IN_PROCEDURE
            SchemeObject* s_body;   // For USER_PROCEDURE
        };
        union {
            int opt;                // For BUILT_IN_PROCEDURE
            SchemeObject* s_formals; // For USER_PROCEDURE
        };

        int rst;                // For BUILT_IN_PROCEDURE
        SchemeObject* (*fn)();  // For BUILT_IN_PROCEDURE

        SchemeObject* envt; // For USER_PROCEDURE

    public:        
        enum ObjectType {
 		    BLANK,                  // Empty slots in heap
		    NUMBER,
		    EMPTY_LIST,
		    SYMBOL,
		    BOOL,
		    CHAR,
		    STRING,
		    VECTOR,
		    PAIR,
		    UNSPECIFIED,
 		    USER_PROCEDURE,
 		    INTERNAL_PROCEDURE,
 		    BUILT_IN_PROCEDURE,
 		    MACRO,
 		    CONTINUATION,
 		    INPUT_PORT,
 		    OUTPUT_PORT,
 		    EOFTYPE,
 		    ENVIRONMENT
		};

    public:
        ObjectType type() const;
        bool immutable() const;
        void set_immutable(bool flag);
        string toString();
        void clear_inuse();
        void mark();
        ~SchemeObject();

        SchemeObject* getVectorElem(int index);
        void setVectorElem(SchemeObject* o, int index);

		SchemeObject* getBinding(SchemeObject* name);
        void putBinding(SchemeObject* name, SchemeObject* o);
        void setBinding(SchemeObject* name, SchemeObject* o);
        
        string nameAsString();
        
        void callContinuation(SchemeObject* arg);
        
        static SchemeObject* createNumber(double number);
        static SchemeObject* createString(char* str);
        static SchemeObject* createChar(char c);
        static SchemeObject* createPair(SchemeObject* car, SchemeObject* cdr);
        static SchemeObject* createVector(SchemeObject* elem, uint32_t length);
        static SchemeObject* createBool(bool b);
        static SchemeObject* createEmptyList();
        static SchemeObject* createUnspecified();
        static SchemeObject* createEOF();
        static SchemeObject* createSymbol(char* str);
        static SchemeObject* createContinuation();
        static SchemeObject* createEnvironment(SchemeObject* parent);
        static SchemeObject* createInputPort(istream* is);
        static SchemeObject* createOutputPort(ostream* os);
        static SchemeObject* createBuiltinProcedure(SchemeObject* name, int req, int opt, int rst, SchemeObject* (*fn)());
        static SchemeObject* createUserProcedure(SchemeObject* name, SchemeObject* envt, SchemeObject* s_formals, SchemeObject* s_body);
        static SchemeObject* createInternalProcedure(SchemeObject* name);
        static SchemeObject* createMacro(SchemeObject* name, SchemeObject* envt, SchemeObject* s_formals, SchemeObject* s_body);

    private:
        static map<char*,SchemeObject*> known_symbols;
        
};

inline
SchemeObject::ObjectType SchemeObject::type() const {
    return ObjectType(type_and_flags & 0x0000ffff);
}

#endif
