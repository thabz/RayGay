
#include "parser/floatnodes.h"

///////////////////////////////////////////////
// FloatNode
///////////////////////////////////////////////

FloatNode::~FloatNode() {
}

///////////////////////////////////////////////
// ModifyNamedFloatNode 
///////////////////////////////////////////////

ModifyNamedFloatNode::ModifyNamedFloatNode(string name, char op, bool before,FilePosition pos) : FloatNode(pos) {
    this->name = name;
    this->op = op;
    this->before = before;
}

ModifyNamedFloatNode::~ModifyNamedFloatNode() {
}

double ModifyNamedFloatNode::eval() {
    double cur = Assignments::getUniqueInstance()->getNamedFloat(name,getFilePosition());
    double result = 0;
    if (before) {
	result = cur;
    }
    if (op == '+') {
	cur += 1;
    } else {
	cur -= 1;
    }
    if (!before) {
		result = cur;
	    }
	    Assignments::getUniqueInstance()->setNamedFloat(name,cur);
	    return result;
	}

