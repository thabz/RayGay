
#ifndef EXCEPTION_H
#define EXCEPTION_H

#include <string>

class Exception {

    public:
	Exception(std::string message) { this->message = message; };
	std::string getMessage() { return message; };
    private:
	std::string message;

};

#endif
