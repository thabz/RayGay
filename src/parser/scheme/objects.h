
#ifndef SCHEME_OBJECTS_H
#define SCHEME_OBJECTS_H

#include <string>

using namespace std;

class SchemeObject {
};

class SchemeNumber : public SchemeObject {
   public:
       SchemeNumber(double number);
       double number;
};

class SchemePair : public SchemeObject {
    public:
	    SchemeObject* car;
     	SchemeObject* cdr;
};

class SchemeString : public SchemeObject {
    public:
        SchemeString(string s);
	    std::string str;

};

class SchemeBool : public SchemeObject {
    public:
        SchemeBool(bool b);
	    bool boolean;
};

#endif
