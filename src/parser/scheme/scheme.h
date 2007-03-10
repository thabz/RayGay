
#ifndef SCHEME_SCHEME_H
#define SCHEME_SCHEME_H

#include <iostream>
#include <string>
#include "objects.h"

using namespace std;

class Scheme {
    public:
        Scheme();
        SchemeObject eval(string code);
        SchemeObject eval(istream* code);

        void assign(string variable, double value);
        void assign(string variable, string value);
        void assign(string variable, bool value);
        
        SchemeObject lookup(string variable);

    private:
        
};

#endif
