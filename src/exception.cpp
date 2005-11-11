
#include "exception.h"

Exception::Exception(std::string message, const char* srcfile, uint32_t srcline) {
    this->message = message;
    this->sourcefile = srcfile;
    this->sourceline = srcline;
}

