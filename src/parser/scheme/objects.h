
#include <string>

class SchemeObject {
};


class SchemeNumber : public SchemeObject {
   public:
       double number;
};

class SchemePair : public SchemeObject {
    public:
	SchemeObject* car;
	SchemeObject* cdr;
};

class SchemeString : public SchemeObject {
    public:
	std::string str;

};

class SchemeBool : public SchemeObject {
    public:
	bool boolean;
};


