
#ifndef EXCEPTION_H
#define EXCEPTION_H

#include <string>

/**
 * This macro is used to actually throw an exception. It is used
 * in order to make sure that source code file and line is stored correctly.
 */
#define throw_exception(s) throw Exception(s,__FILE__,__LINE__)

/**
 * All exceptions throw this class.
 */
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
