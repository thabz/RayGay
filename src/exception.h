
#ifndef EXCEPTION_H
#define EXCEPTION_H

#include <string>

#define throw_exception(s) throw Exception(s,__FILE__,__LINE__)

class Exception {

    public:
	Exception(std::string message, const char* srcfile, unsigned int srcline);
	std::string getMessage() { return message; };
	std::string getSourceFile() { return sourcefile; };
	unsigned int getSourceLine() { return sourceline; };
    private:
	std::string message;
	std::string sourcefile;
	unsigned int sourceline;
};

inline
Exception::Exception(std::string message, const char* srcfile, unsigned int srcline) {
    this->message = message;
    this->sourcefile = srcfile;
    this->sourceline = srcline;
}

#endif
