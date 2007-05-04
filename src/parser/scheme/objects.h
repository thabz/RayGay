
#ifndef SCHEME_OBJECTS_H
#define SCHEME_OBJECTS_H

#include <string>
#include <map>
#include <setjmp.h>

class SchemeEnvironment;

using namespace std;

class SchemeObject {
    public:
	    enum ObjectType {
		    NUMBER,
		    EMPTY_LIST,
		    SYMBOL,
		    BOOL,
		    CHAR,
		    STRING,
		    VECTOR,
		    PAIR,
		    UNSPECIFIED,
 		    PROCEDURE,
 		    INTERNAL_PROCEDURE,
 		    MACRO,
 		    CONTINUATION,
 		    INPUT_PORT,
 		    EOFTYPE,
 		    OUTPUT_PORT,
 		    ENVIRONMENT
		};
	protected:
	    SchemeObject(bool immutable = false);
	public:    
        virtual ~SchemeObject() {};
        virtual string toString() = 0;
        virtual bool boolValue() const { return true; }; // Used in conditional expressions (if, cond, and, or, do)
        virtual ObjectType type() = 0;
	    bool immutable;
	    
	    // For garbage collection
        virtual void mark();
        bool in_use;
};

class SchemeUnspecified : public SchemeObject {
    public:
        SchemeUnspecified() {};
        string toString();
        ObjectType type() { return UNSPECIFIED; };
};

class SchemeNumber : public SchemeObject {
    public:
        static SchemeNumber* create(double number);
        string toString();
        double number;
  	    ObjectType type() { return NUMBER; };
  	private:    
  	    SchemeNumber(double number);
};

class SchemePair : public SchemeObject {
    public:
        static SchemePair* create();
        static SchemePair* create(SchemeObject* car, SchemeObject* cdr);
        ObjectType type() { return PAIR; };
        
        string toString();
	    SchemeObject* car;
     	SchemeObject* cdr;
        void mark();
        
     protected:	
        SchemePair();
        SchemePair(SchemeObject* car, SchemeObject* cdr);
};

class SchemeVector : public SchemeObject {
    public:
        static SchemeVector* create(SchemeObject** elems, int length);
        static SchemeVector* create(SchemeObject* elem, int length);
        ~SchemeVector();
        SchemeObject* get(int index);
        void set(SchemeObject* o, int index);
        ObjectType type() { return VECTOR; };
        void mark();
        
        string toString();
        SchemeObject** elems;
        int length;
    protected:
        SchemeVector(SchemeObject** elems, int length);
        SchemeVector(SchemeObject* elems, int length);
        
};


class SchemeEmptyList : public SchemePair {
    public:
        string toString();
        ObjectType type() { return EMPTY_LIST; };
        void mark();
};

class SchemeSymbol : public SchemeObject {
    public:
        static SchemeSymbol* create(string s);
        string toString();
        ObjectType type() { return SYMBOL; };
	    std::string str;
	private:
        SchemeSymbol(string s);
        static map<string,SchemeSymbol*> known_symbols;    
};


class SchemeString : public SchemeObject {
    public:
        static SchemeString* create(string s, bool immutable = false);
        string toString();
        ObjectType type() { return STRING; };
	    std::string str;
	private:    
        SchemeString(string s, bool immutable);

};

class SchemeBool : public SchemeObject {
    public:
        SchemeBool(bool b);
        string toString();
        ObjectType type() { return BOOL; };
    	bool boolean;
	    bool boolValue() const { return boolean; };
};

class SchemeChar : public SchemeObject {
    public:
        static SchemeChar* create(char c);
        string toString();
        ObjectType type() { return CHAR; };

        char c;
    private:
        SchemeChar(char c);
            
};

class SchemeContinuation : public SchemeObject {
    public:
        SchemeContinuation();
        string toString();
        ObjectType type() { return CONTINUATION; };
        void call(SchemeObject* arg);
        void mark();

        ::jmp_buf jmpbuf;
        SchemeObject* result;
};

class SchemeInputPort : public SchemeObject {
    public:
        SchemeInputPort(istream* is);
        string toString();
        ObjectType type() { return INPUT_PORT; };
        istream* is;
};

class SchemeOutputPort : public SchemeObject {
    public:
        SchemeOutputPort(ostream* os);
        string toString();
        ObjectType type() { return OUTPUT_PORT; };
        ostream* os;
};

class SchemeProcedure : public SchemeObject 
{
    public:
        static SchemeProcedure* create(SchemeObject* name, int req, int opt, int rst, SchemeObject* (*fn)());
        static SchemeProcedure* create(SchemeObject* name, SchemeEnvironment* envt, SchemeObject* s_formals, SchemeObject* s_body);
        string toString();      
        ObjectType type() { return PROCEDURE; };
        string nameAsString() { return name->str; };
        void setName(SchemeObject* name);
        
        void mark();
        
        SchemeSymbol* name;

        // Fields for builtin
        int req;
        int opt;
        int rst;
        SchemeObject* (*fn)();
        
        // Fields for user-function
        SchemeObject* s_body;
        SchemeObject* s_formals;
        SchemeEnvironment* envt;
        
    protected:    
        SchemeProcedure(SchemeObject* name, int req, int opt, int rst, SchemeObject* (*fn)());
        SchemeProcedure::SchemeProcedure(SchemeObject* name, SchemeEnvironment* envt, SchemeObject* s_formals, SchemeObject* s_body);
};

class SchemeInternalProcedure : public SchemeObject {
    public:
        SchemeInternalProcedure(string n) : name(n) {};    
        ObjectType type() { return INTERNAL_PROCEDURE; };
        string toString();      
        
        string name;
};

class SchemeMacro : public SchemeProcedure {
    public:
        static SchemeMacro* create(SchemeObject* name, SchemeEnvironment* envt, SchemeObject* s_formals, SchemeObject* s_body);
        string toString();      
        ObjectType type() { return MACRO; };    
    protected:    
        SchemeMacro(SchemeObject* name, SchemeEnvironment* envt, SchemeObject* s_req, SchemeObject* s_body);
};

class SchemeEOF : public SchemeObject {
    public:
        SchemeEOF();
        string toString();      
        ObjectType type() { return EOFTYPE; };    
    
};

class SchemeEnvironment : public SchemeObject {
    public:
        static SchemeEnvironment* create(SchemeEnvironment* parent);
        string toString();
        ObjectType type() { return ENVIRONMENT; };    

		SchemeObject* get(SchemeObject* name);
        void put(SchemeObject* name, SchemeObject* o);
        void set(SchemeObject* name, SchemeObject* o);
        
        void mark();

	private:
        SchemeEnvironment(SchemeEnvironment* parent);
        SchemeEnvironment* parent;
        map<SchemeObject*,SchemeObject*> binding_map;	
};

#endif
