
#include "parser/function.h"
#include "parser/langnodes.h"

void LangFunction::call(FuncCallArgs* args) {
	    // TODO: Push new scope

	    // Eval each arg and insert into assignments of current scope
	    unsigned int arg_num = arg_decls->size();
	    Assignments* assignments = Assignments::getUniqueInstance();
	    bool status = false;
	    for(unsigned int i = 0; i < arg_num; i++) {
                ValueNode::ValueType type = arg_decls->getType(i);
		string name = arg_decls->getName(i);
		if (type == ValueNode::VECTOR) {
		    Vector v;
		    status = args->getVectorArgValue(i,&v);
		    assignments->setNamedVector(name,v);
		} else if (type == ValueNode::FLOAT) {
		    double v;
		    status = args->getFloatArgValue(i,&v);
		    assignments->setNamedFloat(name,v);
		}
		if (!status) {
		    char line[1000];
		    sprintf(line,"Argument %d has wrong type.",i);
		    throw_exception(line);
		}
	    }

	    // Run the actions
	    actions->eval();

	    // TODO: Pop the scope


}
