
#include "parser/materialnodes.h"

Material* NamedMaterialNode::eval() {
    Material* result;
    result = Assignments::getUniqueInstance()->getNamedMaterial(name);
    if (result == NULL) {
	runtime_error("Material $" + name + " is not defined");
    }
    return result;
}
