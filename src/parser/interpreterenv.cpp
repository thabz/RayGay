
#include "parser/interpreterenv.h"

InterpreterEnv* InterpreterEnv::unique_instance = NULL;

InterpreterEnv* InterpreterEnv::getUniqueInstance() {
    if (unique_instance == NULL) {
	unique_instance = new InterpreterEnv();
    }
    return unique_instance;
}
