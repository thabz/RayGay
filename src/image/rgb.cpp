
#include "rgb.h"

void RGB::clip() {
    for (int i=0; i<3; i++) {
	if (_vector[i] < 0)
	    _vector[i] = 0;
	if (_vector[i] > 1)
	    _vector[i] = 1;
    }
}


