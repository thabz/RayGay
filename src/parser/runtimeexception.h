
#ifndef PARSER_RUNTIME_EXCEPTION_H
#define PARSER_RUNTIME_EXCEPTION_H

#include <string>
#include "parser/fileposition.h"

using namespace std;

class RuntimeException {

    public:
	RuntimeException(string message, FilePosition pos) {
	    this->message = message;
	    this->pos = pos;
	}
	~RuntimeException() {};

	string getMessage() { return message; }
	FilePosition getFilePosition() { return pos; };

    private:
	string message;
	FilePosition pos;
};

#endif
