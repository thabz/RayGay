
#include "parser/materialnodes.h"

Material* NamedMaterialNode::eval() {
    Material* result;
    result = Assignments::getUniqueInstance()->getNamedMaterial(name);
    if (result == NULL) {
	// TODO: Throw exception (with sourceline num) if m == NULL
	throw_exception("Material " + name + " is not defined.");
    }
    return result;
}
