
#ifndef PARSER_FUNCTION_H
#define PARSER_FUNCTION_H

#include <string>
#include <vector>
#include "parser/syntaxnode.h"

class ActionListNode;
class FuncCallArgs;

using namespace std;

class FuncArgsDecls {
    public:
	void addArg(ValueNode::ValueType type, string name) {
	    arg_names.push_back(name);
	    arg_types.push_back(type);
	}
	
	unsigned int size() {
	    return arg_types.size();
	}

	string getName(unsigned int i) {
	    return arg_names[i];
	}

	ValueNode::ValueType getType(unsigned int i) {
	    return arg_types[i];
	}

	vector<string> arg_names;
	vector<ValueNode::ValueType> arg_types;
};


/**
 * A callable function
 */
class Function {

    public: 

	Function(FuncArgsDecls* arg_decls) {
	    this->arg_decls = arg_decls;
	}

	unsigned int getArgsNum() {
	    return arg_decls->size();
	}

	void setActionList(ActionListNode* actions) {
	    this->actions = actions;
	}


	void call(FuncCallArgs* args);

    private:
	ActionListNode* actions;
	FuncArgsDecls* arg_decls;
};

#endif
