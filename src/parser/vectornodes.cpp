
#include "parser/vectornodes.h"

VectorNode::~VectorNode() {
    if (x != NULL) delete x;
    if (y != NULL) delete y;
    if (z != NULL) delete z;
}
