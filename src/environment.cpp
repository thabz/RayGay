
#include "environment.h"

Environment* Environment::unique_instance = NULL;

Environment* Environment::getUniqueInstance() {
    if (unique_instance == NULL) {
	unique_instance = new Environment();
    }
    return unique_instance;
}

