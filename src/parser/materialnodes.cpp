
#include "parser/materialnodes.h"

Material* NamedMaterialNode::eval() {
    Material* result;
    result = Assignments::getUniqueInstance()->getNamedMaterial(name,getFilePosition());
    return result;
}
